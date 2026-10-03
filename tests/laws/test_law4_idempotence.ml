open Vesper
open Vesper.Ast

(** Invariant test for Law 4: Idempotent Normalization & Desugaring. Any normalization,
    canonicalization, or lowering pass must be idempotent: normalize(normalize(A)) ==
    normalize(A) Repeated application of passes reaches a fixed point in a single step
    without cascading mutations or state divergence. *)

let test_constant_folding_arithmetic () =
  let cases =
    [
      ("1 + 2;", "3;");
      ("10 - 4;", "6;");
      ("3 * 7;", "21;");
      ("20 / 4;", "5;");
      ("17 % 5;", "2;");
      ("-(-42);", "42;");
      ("1 + 2 * 3;", "7;");
      ("(1 + 2) * 3;", "9;");
      ("10 - 5 - 2;", "3;");
    ]
  in
  List.iter
    (fun (src, expected_str) ->
      match
        ( Parse_facade.parse_string ~file:"fold.vesper" src,
          Parse_facade.parse_string ~file:"expected.vesper" expected_str )
      with
      | Ok ast, Ok expected_ast -> (
          match (Desugar.lower ast, Desugar.lower expected_ast) with
          | Ok core, Ok expected_core ->
              let norm1 = Normalize.normalize core in
              let norm2 = Normalize.normalize norm1 in
              Alcotest.(check bool)
                "First normalize equals expected" true
                (Core_ir.program_equal norm1 expected_core);
              Alcotest.(check bool)
                "Normalize is idempotent (Law 4)" true
                (Core_ir.program_equal norm1 norm2)
          | _ -> Alcotest.fail "Desugar failed on constant folding test")
      | _ -> Alcotest.fail "Parse failed on constant folding test")
    cases

let test_constant_folding_booleans_and_comparisons () =
  let cases =
    [
      ("!true;", "false;");
      ("!false;", "true;");
      ("1 == 1;", "true;");
      ("1 == 2;", "false;");
      ("1 != 2;", "true;");
      ("2 < 3;", "true;");
      ("3 <= 3;", "true;");
      ("5 > 2;", "true;");
      ("4 >= 5;", "false;");
      ("\"hello\" == \"hello\";", "true;");
      ("\"a\" != \"b\";", "true;");
    ]
  in
  List.iter
    (fun (src, expected_str) ->
      match
        ( Parse_facade.parse_string ~file:"bool_fold.vesper" src,
          Parse_facade.parse_string ~file:"expected_bool.vesper" expected_str )
      with
      | Ok ast, Ok expected_ast -> (
          match (Desugar.lower ast, Desugar.lower expected_ast) with
          | Ok core, Ok expected_core ->
              let norm1 = Normalize.normalize core in
              let norm2 = Normalize.normalize norm1 in
              Alcotest.(check bool)
                "Folded boolean matches expected" true
                (Core_ir.program_equal norm1 expected_core);
              Alcotest.(check bool)
                "Normalize is idempotent" true
                (Core_ir.program_equal norm1 norm2)
          | _ -> Alcotest.fail "Desugar failed")
      | _ -> Alcotest.fail "Parse failed")
    cases

let test_dead_code_elimination () =
  let cases =
    [
      ("if true then 10 else 20;", "10;");
      ("if false then 10 else 20;", "20;");
      ("if 1 == 1 then 42 else 0;", "42;");
      ("if 2 > 5 then 1 else 2;", "2;");
      ("let x = if true then 1 + 2 else 99; x;", "let x = 3; x;");
    ]
  in
  List.iter
    (fun (src, expected_str) ->
      match
        ( Parse_facade.parse_string ~file:"dce.vesper" src,
          Parse_facade.parse_string ~file:"expected_dce.vesper" expected_str )
      with
      | Ok ast, Ok expected_ast -> (
          match (Desugar.lower ast, Desugar.lower expected_ast) with
          | Ok core, Ok expected_core ->
              let norm1 = Normalize.normalize core in
              let norm2 = Normalize.normalize norm1 in
              Alcotest.(check bool)
                "DCE matches expected" true
                (Core_ir.program_equal norm1 expected_core);
              Alcotest.(check bool)
                "DCE normalize is idempotent" true
                (Core_ir.program_equal norm1 norm2)
          | _ -> Alcotest.fail "Desugar failed")
      | _ -> Alcotest.fail "Parse failed")
    cases

let test_safe_division_by_zero () =
  (* Under Law 1, constant folding must not crash on division by zero *)
  let src = "let x = 10 / 0;" in
  match Parse_facade.parse_string ~file:"divzero.vesper" src with
  | Error err -> Alcotest.fail err.message
  | Ok ast -> (
      match Desugar.lower ast with
      | Error err -> Alcotest.fail err.message
      | Ok core ->
          let norm1 = Normalize.normalize core in
          let norm2 = Normalize.normalize norm1 in
          Alcotest.(check bool)
            "Division by zero does not crash and is idempotent" true
            (Core_ir.program_equal norm1 norm2))

let test_fuzz_1000_idempotence () =
  Random.init 42;
  for i = 1 to 1000 do
    let ast_prog = Test_law2_roundtrip.gen_program 3 in
    match Desugar.lower ast_prog with
    | Error err ->
        Alcotest.fail
          (Printf.sprintf "Desugar failed on fuzz iteration %d: %s" i err.message)
    | Ok core_prog -> (
        let norm1 = Normalize.normalize core_prog in
        let norm2 = Normalize.normalize norm1 in
        if not (Core_ir.program_equal norm1 norm2) then
          Alcotest.fail
            (Printf.sprintf
               "Law 4 Idempotence violated on iteration %d!\nPass 1:\n%s\nPass 2:\n%s\n" i
               (Core_ir.to_string norm1) (Core_ir.to_string norm2));
        (* Also verify that validation succeeds on both passes *)
        (match Core_ir.validate_program norm1 with
        | Ok () -> ()
        | Error err ->
            Alcotest.fail
              (Printf.sprintf "Norm1 validation failed: %s at %s" err.message
                 (span_to_string err.span)));
        match Core_ir.validate_program norm2 with
        | Ok () -> ()
        | Error err ->
            Alcotest.fail
              (Printf.sprintf "Norm2 validation failed: %s at %s" err.message
                 (span_to_string err.span)))
  done

let tests =
  [
    Alcotest.test_case "Constant folding arithmetic" `Quick
      test_constant_folding_arithmetic;
    Alcotest.test_case "Constant folding booleans and comparisons" `Quick
      test_constant_folding_booleans_and_comparisons;
    Alcotest.test_case "Dead code elimination on conditional branches" `Quick
      test_dead_code_elimination;
    Alcotest.test_case "Safe handling of division by zero (Law 1 isolation)" `Quick
      test_safe_division_by_zero;
    Alcotest.test_case "1,000 random programs satisfy single-step idempotence (Law 4)"
      `Quick test_fuzz_1000_idempotence;
  ]
