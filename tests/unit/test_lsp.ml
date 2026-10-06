open Vesper

let json s =
  match Json.parse s with Ok j -> j | Error e -> Alcotest.fail ("bad test json: " ^ e)

let send st s =
  let st', out = Lsp_server.handle_message st (json s) in
  (st', out)

let did_open uri text =
  Json.to_string
    (Json.Object
       [
         ("jsonrpc", Json.String "2.0");
         ("method", Json.String "textDocument/didOpen");
         ( "params",
           Json.Object
             [
               ( "textDocument",
                 Json.Object
                   [
                     ("uri", Json.String uri);
                     ("languageId", Json.String "vesper");
                     ("version", Json.Int 1);
                     ("text", Json.String text);
                   ] );
             ] );
       ])

let did_change uri text =
  Json.to_string
    (Json.Object
       [
         ("jsonrpc", Json.String "2.0");
         ("method", Json.String "textDocument/didChange");
         ( "params",
           Json.Object
             [
               ("textDocument", Json.Object [ ("uri", Json.String uri) ]);
               ("contentChanges", Json.List [ Json.Object [ ("text", Json.String text) ] ]);
             ] );
       ])

let diag_count out =
  match out with
  | [ n ] -> (
      match
        Option.bind (Json.member "params" n) (fun p ->
            Option.bind (Json.member "diagnostics" p) Json.to_list_opt)
      with
      | Some l -> List.length l
      | None -> Alcotest.fail "no diagnostics array")
  | _ -> Alcotest.fail "expected exactly one notification"

let test_json_roundtrip () =
  let src = {|{"a":[1,2,{"b":null}],"s":"x\n\"y\"","t":true,"f":1.5}|} in
  (match Json.parse src with
  | Ok j -> Alcotest.(check string) "roundtrip" src (Json.to_string j)
  | Error e -> Alcotest.fail e);
  List.iter
    (fun bad ->
      match Json.parse bad with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "should reject %S" bad)
    [ ""; "{"; "[1,"; "{\"a\" 1}"; "tru"; "\"abc"; "1 2"; "{\"a\":}" ]

let test_initialize () =
  let st, out =
    send Lsp_server.initial {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|}
  in
  ignore st;
  match out with
  | [ r ] ->
      let caps = Option.bind (Json.member "result" r) (Json.member "capabilities") in
      Alcotest.(check bool)
        "formatting advertised" true
        (Option.bind caps (Json.member "documentFormattingProvider")
        = Some (Json.Bool true))
  | _ -> Alcotest.fail "expected one response"

let test_diagnostics_on_open_and_change () =
  let uri = "file:///t.vesper" in
  let st, out = send Lsp_server.initial (did_open uri "let x = 1 +;\n") in
  Alcotest.(check bool) "syntax error reported" true (diag_count out >= 1);
  let st, out = send st (did_open uri "let x = 1 + true;\n") in
  Alcotest.(check bool) "type error reported" true (diag_count out >= 1);
  let _, out = send st (did_change uri "let x = 1 + 2;\n") in
  Alcotest.(check int) "clean after change" 0 (diag_count out)

let test_formatting_matches_printer () =
  let uri = "file:///f.vesper" in
  let text = "let   x=1+2;\nlet f a   = a*x;\n" in
  let st, _ = send Lsp_server.initial (did_open uri text) in
  let _, out =
    send st
      (Printf.sprintf
         {|{"jsonrpc":"2.0","id":7,"method":"textDocument/formatting","params":{"textDocument":{"uri":%S},"options":{"tabSize":2,"insertSpaces":true}}}|}
         uri)
  in
  let expected =
    match Parse_facade.parse_string ~file:uri text with
    | Ok p -> Printer.to_string p
    | Error _ -> Alcotest.fail "parse"
  in
  match out with
  | [ r ] -> (
      match Option.bind (Json.member "result" r) Json.to_list_opt with
      | Some [ edit ] ->
          Alcotest.(check (option string))
            "newText equals Printer.to_string" (Some expected)
            (Option.bind (Json.member "newText" edit) Json.to_string_opt)
      | _ -> Alcotest.fail "expected one edit")
  | _ -> Alcotest.fail "expected one response"

let test_robustness () =
  let st = Lsp_server.initial in
  let _, out = send st {|{"jsonrpc":"2.0","id":3,"method":"nope"}|} in
  (match out with
  | [ r ] ->
      Alcotest.(check (option int))
        "method not found" (Some (-32601))
        (Option.bind (Json.member "error" r) (fun e ->
             Option.bind (Json.member "code" e) Json.to_int_opt))
  | _ -> Alcotest.fail "expected error response");
  List.iter
    (fun m -> ignore (send st m))
    [
      {|{}|};
      {|[]|};
      {|{"method":"textDocument/didOpen"}|};
      {|{"method":"textDocument/didChange","params":{"contentChanges":5}}|};
      {|{"id":1,"method":"textDocument/formatting","params":{}}|};
      {|{"id":2,"method":"textDocument/formatting","params":{"textDocument":{"uri":"x"}}}|};
    ]

let test_lifecycle () =
  let st, _ = send Lsp_server.initial {|{"id":1,"method":"shutdown"}|} in
  let st, _ = send st {|{"method":"exit"}|} in
  Alcotest.(check (option int)) "clean exit" (Some 0) (Lsp_server.exit_code st);
  let st, _ = send Lsp_server.initial {|{"method":"exit"}|} in
  Alcotest.(check (option int)) "unclean exit" (Some 1) (Lsp_server.exit_code st)

let () =
  Alcotest.run "Vesper LSP Unit Tests"
    [
      ( "LSP",
        [
          Alcotest.test_case "JSON" `Quick test_json_roundtrip;
          Alcotest.test_case "initialize" `Quick test_initialize;
          Alcotest.test_case "diagnostics" `Quick test_diagnostics_on_open_and_change;
          Alcotest.test_case "formatting" `Quick test_formatting_matches_printer;
          Alcotest.test_case "robustness" `Quick test_robustness;
          Alcotest.test_case "lifecycle" `Quick test_lifecycle;
        ] );
    ]
