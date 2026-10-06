open Vesper

(** Phase 10: tooling must preserve Laws 1, 2 and 5. *)

let sources =
  [
    "let x = 1 + 2;";
    "let   f a b=a*b+1 ;\nf 2 3;";
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);\nfact 5;";
    "let s = \"hi\";\nlet t = not true;";
  ]

let test_fmt_idempotent () =
  List.iter
    (fun src ->
      match Cli.format_source ~file:"<t>" src with
      | Error _ -> Alcotest.fail "format failed"
      | Ok once -> (
          match Cli.format_source ~file:"<t>" once with
          | Ok twice -> Alcotest.(check string) "fmt idempotent" once twice
          | Error _ -> Alcotest.fail "formatted output does not reparse"))
    sources

let test_fmt_preserves_ast () =
  List.iter
    (fun src ->
      match
        (Parse_facade.parse_string ~file:"<t>" src, Cli.format_source ~file:"<t>" src)
      with
      | Ok a, Ok out -> (
          match Parse_facade.parse_string ~file:"<t>" out with
          | Ok b -> Alcotest.(check bool) "ast preserved" true (Ast.ast_equal a b)
          | Error _ -> Alcotest.fail "reparse failed")
      | _ -> Alcotest.fail "parse failed")
    sources

let test_repl_deterministic () =
  let run () =
    List.fold_left
      (fun (st, acc) src ->
        match Repl.eval_input st src with
        | Ok (st', out) -> (st', out :: acc)
        | Error _ -> (st, "ERR" :: acc))
      (Repl.initial (), [])
      [ "let x = 5;"; "x * x;"; "1 / 0;"; "let f a = a + x;"; "f 1;" ]
    |> snd
  in
  Alcotest.(check (list string)) "identical replays" (run ()) (run ())

let test_total_on_garbage () =
  let garbage =
    [ ""; ";"; "let"; "((("; "\000\255"; "let x = \"abc"; "/*"; String.make 5000 '(' ]
  in
  List.iter
    (fun g ->
      ignore (Repl.eval_input (Repl.initial ()) g);
      ignore (Lsp_server.diagnostics ~file:"<g>" g);
      ignore (Json.parse g);
      ignore (Cli.parse_args [| "vesper"; g |]);
      ignore (Cli.format_source ~file:"<g>" g))
    garbage

let tests =
  [
    Alcotest.test_case "fmt idempotent (Law 4)" `Quick test_fmt_idempotent;
    Alcotest.test_case "fmt preserves AST (Law 2)" `Quick test_fmt_preserves_ast;
    Alcotest.test_case "REPL deterministic (Law 5)" `Quick test_repl_deterministic;
    Alcotest.test_case "tooling total on garbage (Law 1)" `Quick test_total_on_garbage;
  ]
