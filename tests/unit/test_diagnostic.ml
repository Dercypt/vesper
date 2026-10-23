open Vesper

let dummy_span =
  match
    Ast.create_span ~file:"test.vesper" ~start_line:1 ~start_col:0 ~end_line:1 ~end_col:5
  with
  | Ok s -> s
  | Error _ -> Ast.dummy_span

let test_compiler_primop_arity_error () =
  (* Construct an invalid primop arity expression in Core IR *)
  let invalid_expr =
    Core_ir.make_expr ~span:dummy_span
      (Core_ir.PrimOp
         {
           op = Core_ir.Add;
           args = [ Core_ir.make_expr ~span:dummy_span (Core_ir.Lit (Ast.Int 1)) ];
         })
  in
  match Compiler.compile_expr invalid_expr with
  | Ok _ -> Alcotest.fail "Expected compilation error on invalid primop arity"
  | Error err ->
      Alcotest.(check bool)
        "Ast.error code is Some E0002_internal_error" true
        (match err.code with
        | Some c -> Error_code.equal c Error_code.E0002_internal_error
        | None -> false);
      let diag = Diagnostic.of_ast_error err in
      Alcotest.(check bool)
        "Diagnostic code is E0002_internal_error (not E1004_invalid_escape)" true
        (Error_code.equal diag.code Error_code.E0002_internal_error);
      Alcotest.(check bool)
        "Diagnostic does not misreport as E1004" false
        (Error_code.equal diag.code Error_code.E1004_invalid_escape);
      Alcotest.(check (option string))
        "Hint is None (no escape sequence hint)" None diag.hint

let test_roundtrip () =
  let diag =
    Diagnostic.error ~code:Error_code.E2001_type_error ~span:dummy_span "Type mismatch"
  in
  let ast_err = Diagnostic.to_ast_error diag in
  Alcotest.(check bool)
    "to_ast_error retains code" true
    (match ast_err.code with
    | Some c -> Error_code.equal c Error_code.E2001_type_error
    | None -> false);
  let diag' = Diagnostic.of_ast_error ast_err in
  Alcotest.(check bool)
    "of_ast_error reconstructs exact code" true
    (Error_code.equal diag'.code Error_code.E2001_type_error)

let test_precedence_and_fallback () =
  (* Explicit ~code parameter takes highest precedence *)
  let err_with_code =
    Ast.make_error ~code:Error_code.E1005_unexpected_char ~span:dummy_span
      "Unexpected character"
  in
  let diag_override =
    Diagnostic.of_ast_error ~code:Error_code.E0001_io_error err_with_code
  in
  Alcotest.(check bool)
    "Explicit ~code overrides err.code" true
    (Error_code.equal diag_override.code Error_code.E0001_io_error);

  (* err.code is used when ?code is omitted *)
  let diag_from_err = Diagnostic.of_ast_error err_with_code in
  Alcotest.(check bool)
    "err.code is preserved when ?code is None" true
    (Error_code.equal diag_from_err.code Error_code.E1005_unexpected_char);

  (* Fallback to E1008_syntax_error when code is completely absent *)
  let err_no_code =
    { Ast.code = None; span = dummy_span; message = "Random syntax issue" }
  in
  let diag_fallback = Diagnostic.of_ast_error err_no_code in
  Alcotest.(check bool)
    "Fallback to E1008_syntax_error" true
    (Error_code.equal diag_fallback.code Error_code.E1008_syntax_error)

let test_lexer_error_codes () =
  let check_code src expected_code =
    match Parse_facade.parse_string ~file:"test.vesper" src with
    | Ok _ -> Alcotest.fail ("Expected failure for: " ^ src)
    | Error diag ->
        Alcotest.(check bool)
          ("Error code matches for " ^ src)
          true
          (Error_code.equal diag.code expected_code)
  in
  check_code "\"unterminated string" Error_code.E1002_unterminated_string;
  check_code "/* unclosed comment" Error_code.E1003_unclosed_comment;
  check_code "\"invalid \\q escape\"" Error_code.E1004_invalid_escape;
  check_code "@" Error_code.E1005_unexpected_char;
  check_code "9999999999999999999999999999999999" Error_code.E1006_integer_overflow

let () =
  Alcotest.run "Diagnostic Tests"
    [
      ( "code_inference",
        [
          Alcotest.test_case "Compiler primop arity does not misreport as escape error"
            `Quick test_compiler_primop_arity_error;
          Alcotest.test_case "Roundtrip between Diagnostic.t and Ast.error" `Quick
            test_roundtrip;
          Alcotest.test_case "Precedence and fallback behavior" `Quick
            test_precedence_and_fallback;
          Alcotest.test_case "Lexer error codes attached directly" `Quick
            test_lexer_error_codes;
        ] );
    ]
