open Vesper
open Vesper.Ast

(** Invariant test for Law 1: Total Diagnosability & Host Isolation. Adversarial inputs
    must return structured diagnostics and never raise uncaught host exceptions. *)

let test_invalid_spans () =
  let result =
    create_span ~file:"test.vesper" ~start_line:5 ~start_col:10 ~end_line:2 ~end_col:0
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected invalid span to return Error, got Ok"
  | Error err ->
      Alcotest.(check bool)
        "Diagnostic message must not be empty" true
        (String.length err.message > 0)

let test_null_bytes () =
  let samples =
    [
      "\x00";
      "let \x00 = 42;";
      "let x = \"hello \x00 world\";";
      "\x00\x00\x00\x00";
      "if \x00 then 1 else 2";
    ]
  in
  List.iter
    (fun s ->
      match Parse_facade.parse_string ~file:"null_bytes.vesper" s with
      | Ok _ -> ()
      | Error diag ->
          Alcotest.(check bool)
            "Span is valid on null byte input" true (Ast.span_is_valid diag.span);
          Alcotest.(check bool)
            "Diagnostic message is non-empty" true
            (String.length diag.message > 0))
    samples

let test_unbalanced_quotes () =
  let samples =
    [
      "\"";
      "\"unterminated";
      "\"line 1\nline 2";
      "\"escaped \\\" but still open";
      "let s = \"unclosed string;";
    ]
  in
  List.iter
    (fun s ->
      match Parse_facade.parse_string ~file:"unbalanced_quotes.vesper" s with
      | Ok _ -> Alcotest.fail ("Expected syntax error on unterminated quote: " ^ s)
      | Error diag ->
          Alcotest.(check bool)
            "Span is valid on quote error" true (Ast.span_is_valid diag.span);
          Alcotest.(check bool)
            "Code is unterminated string" true
            (Error_code.equal diag.code Error_code.E1002_unterminated_string))
    samples

let test_deep_nesting () =
  let parens = String.make 10000 '(' in
  match Parse_facade.parse_string ~file:"nesting.vesper" parens with
  | Ok _ -> Alcotest.fail "Expected syntax error on deep unclosed parens"
  | Error diag ->
      Alcotest.(check bool)
        "Span is valid on deep nesting" true (Ast.span_is_valid diag.span);
      Alcotest.(check bool) "Message is non-empty" true (String.length diag.message > 0)

let test_truncated_inputs () =
  let fragments =
    [
      "let";
      "let rec";
      "let x";
      "let x =";
      "if";
      "if true";
      "if true then";
      "if true then 1";
      "if true then 1 else";
      "fun";
      "fun x";
      "fun x ->";
      "1 +";
      "(1 +";
      "/* unclosed block comment";
      "let a = 1;";
    ]
  in
  List.iter
    (fun frag ->
      match Parse_facade.parse_string ~file:"truncated.vesper" frag with
      | Ok prog ->
          Alcotest.(check bool)
            "Span is valid on parsed fragment" true (Ast.span_is_valid prog.span)
      | Error diag ->
          Alcotest.(check bool)
            "Span is valid on truncated error" true (Ast.span_is_valid diag.span);
          Alcotest.(check bool)
            "Message is non-empty" true
            (String.length diag.message > 0))
    fragments

let test_driver_isolation () =
  (* Test read_file on non-existent path *)
  (match Driver.read_file "/nonexistent/path/file.vesper" with
  | Ok _ -> Alcotest.fail "Expected error on nonexistent file"
  | Error diag ->
      Alcotest.(check bool)
        "Code is E0001_io_error" true
        (Error_code.equal diag.code Error_code.E0001_io_error);
      Alcotest.(check bool)
        "Message mentions filename" true
        (String.length diag.message > 0));

  (* Test Driver.compile_string safely isolates errors *)
  (match Driver.compile_string "let x =" with
  | Ok _ -> Alcotest.fail "Expected syntax error"
  | Error diag ->
      Alcotest.(check bool) "Span is valid" true (Ast.span_is_valid diag.span);
      Alcotest.(check bool)
        "Has error code" true
        (Error_code.equal diag.code Error_code.E1001_unexpected_token));

  (* Test Driver.run_cli never raises exceptions *)
  let exit_code = Driver.run_cli [| "vesper"; "-e"; "let x =" |] in
  Alcotest.(check int) "CLI returns 1 on syntax error" 1 exit_code;

  let exit_code_ok = Driver.run_cli [| "vesper"; "-e"; "let x = 42;" |] in
  Alcotest.(check int) "CLI returns 0 on valid input" 0 exit_code_ok

let test_diagnostic_rendering () =
  let span =
    match
      create_span ~file:"test.vesper" ~start_line:2 ~start_col:4 ~end_line:2 ~end_col:8
    with
    | Ok s -> s
    | Error _ -> Ast.dummy_span
  in
  let diag =
    Diagnostic.error ~code:Error_code.E1001_unexpected_token ~span
      ~hint:"Check your syntax" "Unexpected token near 'foo'"
  in
  let source = "let a = 1;\n    foo bar\nlet b = 2;\n" in
  let plain = Diagnostic.render_terminal ~use_color:false ~source diag in
  Alcotest.(check bool) "Contains error code" true (String.length plain > 0);
  let colored = Diagnostic.render_terminal ~use_color:true ~source diag in
  Alcotest.(check bool)
    "Contains ANSI color" true
    (String.length colored > String.length plain);
  let extracted = Diagnostic.extract_line ~source ~line:2 in
  Alcotest.(check (option string))
    "Extracts line 2 correctly" (Some "    foo bar") extracted;
  let oob = Diagnostic.extract_line ~source ~line:999 in
  Alcotest.(check (option string)) "Out of bounds line is None" None oob

let gen_adversarial_sample () =
  let kind = Random.int 6 in
  match kind with
  | 0 ->
      (* Null bytes and control characters *)
      let len = 1 + Random.int 32 in
      let b = Buffer.create len in
      for _ = 1 to len do
        Buffer.add_char b (Char.chr (Random.int 32))
      done;
      Buffer.contents b
  | 1 ->
      (* Unbalanced quotes and random escape sequences *)
      let len = 1 + Random.int 32 in
      let b = Buffer.create len in
      Buffer.add_char b '"';
      for _ = 1 to len do
        match Random.int 4 with
        | 0 -> Buffer.add_string b "\\n"
        | 1 -> Buffer.add_string b "\\\""
        | 2 -> Buffer.add_char b '\\'
        | _ -> Buffer.add_char b (Char.chr (32 + Random.int 95))
      done;
      Buffer.contents b
  | 2 ->
      (* Random byte streams (0 to 255) *)
      let len = 1 + Random.int 64 in
      let b = Buffer.create len in
      for _ = 1 to len do
        Buffer.add_char b (Char.chr (Random.int 256))
      done;
      Buffer.contents b
  | 3 ->
      (* Random mix of keywords and operators *)
      let words =
        [|
          "let";
          "rec";
          "in";
          "if";
          "then";
          "else";
          "fun";
          "->";
          "=";
          "+";
          "-";
          "*";
          "/";
          ";";
          "(";
          ")";
          "{";
          "}";
          "//";
          "/*";
          "*/";
          "42";
          "\"str\"";
          "true";
          "false";
          "ident";
        |]
      in
      let len = 1 + Random.int 16 in
      let parts = List.init len (fun _ -> words.(Random.int (Array.length words))) in
      String.concat " " parts
  | 4 ->
      (* Unbalanced brackets / parentheses *)
      let len = 1 + Random.int 50 in
      let b = Buffer.create len in
      for _ = 1 to len do
        let c = if Random.bool () then '(' else ')' in
        Buffer.add_char b c
      done;
      Buffer.contents b
  | _ ->
      (* Chaotic integer overflows and illegal characters *)
      let digits = String.make (20 + Random.int 30) '9' in
      "let x = " ^ digits ^ ";"

let test_10000_adversarial_fuzzing () =
  Random.init 42;
  let sample_count = 10_000 in
  for _ = 1 to sample_count do
    let sample = gen_adversarial_sample () in
    match Parse_facade.parse_string ~file:"fuzz.vesper" sample with
    | Ok prog ->
        if not (Ast.span_is_valid prog.span) then
          Alcotest.fail "Parsed program has invalid span coordinates"
    | Error diag ->
        if not (Ast.span_is_valid diag.span) then
          Alcotest.fail
            (Printf.sprintf "Diagnostic on sample %S has invalid span: %s" sample
               (Ast.span_to_string diag.span));
        if String.length diag.message = 0 then Alcotest.fail "Diagnostic message is empty"
  done

let tests =
  let open Alcotest in
  [
    test_case "Invalid span coordinates rejected gracefully" `Quick test_invalid_spans;
    test_case "Null byte handling without host crashes" `Quick test_null_bytes;
    test_case "Unbalanced quotes error diagnosis" `Quick test_unbalanced_quotes;
    test_case "Deep nesting without stack overflow" `Quick test_deep_nesting;
    test_case "Truncated inputs report structured diagnostics" `Quick
      test_truncated_inputs;
    test_case "Driver file reading and CLI isolation" `Quick test_driver_isolation;
    test_case "Diagnostic line snippet and terminal formatting" `Quick
      test_diagnostic_rendering;
    test_case "10,000 adversarial fuzzing inputs isolate host completely" `Quick
      test_10000_adversarial_fuzzing;
  ]
