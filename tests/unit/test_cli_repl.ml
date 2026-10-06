open Vesper

let parse l = Cli.parse_args (Array.of_list ("vesper" :: l))
let is_error = function Error _ -> true | Ok _ -> false

let test_parse_args () =
  Alcotest.(check bool) "run" true (parse [ "run"; "a.vesper" ] = Ok (Cli.Run "a.vesper"));
  Alcotest.(check bool) "check" true (parse [ "check"; "a" ] = Ok (Cli.Check "a"));
  Alcotest.(check bool) "disasm" true (parse [ "disasm"; "a" ] = Ok (Cli.Disasm "a"));
  Alcotest.(check bool) "repl" true (parse [ "repl" ] = Ok Cli.Repl);
  Alcotest.(check bool) "help" true (parse [ "--help" ] = Ok Cli.Help);
  Alcotest.(check bool) "version" true (parse [ "--version" ] = Ok Cli.Version);
  Alcotest.(check bool) "no args" true (parse [] = Ok Cli.Help);
  Alcotest.(check bool)
    "fmt stdout" true
    (parse [ "fmt"; "a" ] = Ok (Cli.Fmt { file = "a"; write = false }));
  Alcotest.(check bool)
    "fmt write" true
    (parse [ "fmt"; "--write"; "a" ] = Ok (Cli.Fmt { file = "a"; write = true }));
  Alcotest.(check bool)
    "legacy file" true
    (match parse [ "a.vesper" ] with Ok (Cli.Legacy _) -> true | _ -> false);
  List.iter
    (fun args ->
      Alcotest.(check bool) (String.concat " " args) true (is_error (parse args)))
    [
      [ "run" ];
      [ "run"; "a"; "b" ];
      [ "run"; "--x" ];
      [ "repl"; "x" ];
      [ "-e" ];
      [ "--zzz" ];
      [ "fmt" ];
    ]

let test_repl_state () =
  let st = Repl.initial () in
  match Repl.eval_input st "let x = 40 + 2;" with
  | Error _ -> Alcotest.fail "let failed"
  | Ok (st, line) -> (
      Alcotest.(check string) "let output" "val x : int = 42" line;
      match Repl.eval_input st "x + 1;" with
      | Ok (_, line) -> Alcotest.(check string) "expr output" "- : int = 43" line
      | Error _ -> Alcotest.fail "expr failed")

let test_repl_errors () =
  let st = Repl.initial () in
  Alcotest.(check bool) "syntax" true (is_error (Repl.eval_input st "let = ;"));
  Alcotest.(check bool) "type" true (is_error (Repl.eval_input st "1 + true;"));
  Alcotest.(check bool) "runtime" true (is_error (Repl.eval_input st "1 / 0;"));
  Alcotest.(check bool) "unbound" true (is_error (Repl.eval_input st "nope;"))

let test_repl_run () =
  let lines = ref [ "let x = 1;"; "let y ="; "  x + 1;"; "bad bad ;"; "y;" ] in
  let read_line () =
    match !lines with
    | [] -> None
    | l :: tl ->
        lines := tl;
        Some l
  in
  let out = Buffer.create 64 and err = Buffer.create 64 in
  let code =
    Repl.run ~read_line ~out:(Buffer.add_string out) ~err:(Buffer.add_string err)
  in
  Alcotest.(check int) "exit code" 0 code;
  Alcotest.(check bool)
    "final value printed" true
    (let s = Buffer.contents out in
     let needle = "- : int = 2" in
     let rec find i =
       i + String.length needle <= String.length s
       && (String.sub s i (String.length needle) = needle || find (i + 1))
     in
     find 0);
  Alcotest.(check bool) "error on stderr" true (Buffer.length err > 0)

let () =
  Alcotest.run "Vesper CLI & REPL Unit Tests"
    [
      ( "CLI & REPL",
        [
          Alcotest.test_case "argument parsing" `Quick test_parse_args;
          Alcotest.test_case "repl state" `Quick test_repl_state;
          Alcotest.test_case "repl errors" `Quick test_repl_errors;
          Alcotest.test_case "repl loop" `Quick test_repl_run;
        ] );
    ]
