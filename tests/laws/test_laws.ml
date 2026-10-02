open Vesper.Ast

(** Invariant test for Law 1: Total Diagnosability & Host Isolation. Adversarial inputs
    must return structured Result.Error and never raise uncaught host exceptions. *)
let test_law1_no_host_crashes_on_invalid_spans () =
  let result =
    create_span ~file:"test.vesper" ~start_line:5 ~start_col:10 ~end_line:2 ~end_col:0
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected invalid span to return Error, got Ok"
  | Error err ->
      Alcotest.(check bool)
        "Diagnostic message must not be empty" true
        (String.length err.message > 0)

(** Invariant test for Law 3: Strict Source Provenance. Valid AST nodes must preserve
    source span coordinates across representations. *)
let test_law3_source_provenance_retention () =
  match
    create_span ~file:"main.vesper" ~start_line:1 ~start_col:0 ~end_line:1 ~end_col:4
  with
  | Error err -> Alcotest.fail ("Failed to create valid span: " ^ err.message)
  | Ok span ->
      let expr = { span; desc = Lit (Int 42) } in
      (match validate_expr expr with
      | Error err -> Alcotest.fail ("Expression failed validation: " ^ err.message)
      | Ok () -> ());
      Alcotest.(check string) "Span file is preserved" "main.vesper" expr.span.file;
      Alcotest.(check int) "Span start line preserved" 1 expr.span.start_line;
      Alcotest.(check int) "Span end col preserved" 4 expr.span.end_col

let () =
  let open Alcotest in
  run "Vesper Domain Invariants"
    [
      ( "Law 1: Total Diagnosability & Host Isolation",
        [
          test_case "Adversarial input yields Result.Error without panic" `Quick
            test_law1_no_host_crashes_on_invalid_spans;
        ] );
      ( "Law 3: Strict Source Provenance",
        [
          test_case "Valid AST nodes preserve source location spans" `Quick
            test_law3_source_provenance_retention;
        ] );
    ]
