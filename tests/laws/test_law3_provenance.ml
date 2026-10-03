open Vesper
open Vesper.Ast

(** Invariant test for Law 3: Strict Source Provenance (Span Retention). Every AST node,
    desugared intermediate representation (IR) node, and diagnostic artifact must carry a
    valid, traceable source span. Fabricated or desugared nodes must never carry dummy
    spans. *)

let rec assert_expr_provenance (e : Core_ir.expr) : unit =
  if not (span_is_valid e.span) then
    Alcotest.fail
      (Printf.sprintf "Core IR expression violates Law 3 with invalid span: %s"
         (span_to_string e.span));
  if e.span = dummy_span then
    Alcotest.fail "Core IR expression has dummy_span; provenance was lost!";
  match e.desc with
  | Lit _ | Var _ -> ()
  | Fun { param = _; body } -> assert_expr_provenance body
  | App { fn; arg } ->
      assert_expr_provenance fn;
      assert_expr_provenance arg
  | Let { name = _; is_rec = _; value; body } ->
      assert_expr_provenance value;
      assert_expr_provenance body
  | PrimOp { op = _; args } -> List.iter assert_expr_provenance args
  | If { cond; then_branch; else_branch } ->
      assert_expr_provenance cond;
      assert_expr_provenance then_branch;
      assert_expr_provenance else_branch

let assert_decl_provenance (d : Core_ir.decl) : unit =
  if not (span_is_valid d.span) then
    Alcotest.fail
      (Printf.sprintf "Core IR declaration violates Law 3 with invalid span: %s"
         (span_to_string d.span));
  if d.span = dummy_span then
    Alcotest.fail "Core IR declaration has dummy_span; provenance was lost!";
  match d.desc with
  | LetDecl { name = _; is_rec = _; value } -> assert_expr_provenance value
  | ExprDecl e -> assert_expr_provenance e

let assert_program_provenance (p : Core_ir.program) : unit =
  if not (span_is_valid p.span) then
    Alcotest.fail
      (Printf.sprintf "Core IR program violates Law 3 with invalid span: %s"
         (span_to_string p.span));
  if p.span = dummy_span then
    Alcotest.fail "Core IR program has dummy_span; provenance was lost!";
  List.iter assert_decl_provenance p.decls;
  match Core_ir.validate_program p with
  | Ok () -> ()
  | Error err ->
      Alcotest.fail
        (Printf.sprintf "Core_ir.validate_program failed: %s at %s" err.message
           (span_to_string err.span))

let test_handcrafted_provenance () =
  let samples =
    [
      "let x = 42;";
      "let f x y z = x + y * z;";
      "let rec fact n = if n == 0 then 1 else n * fact (n - 1);";
      "fun a b -> a && b || true;";
      "if a && b then 1 else 2;";
      "let x = 10 in let y = 20 in x + y;";
      "let g = fun x -> fun y -> x - y;";
      "let test a b = if !a then b else -b;";
    ]
  in
  List.iter
    (fun src ->
      match Parse_facade.parse_string ~file:"provenance_test.vesper" src with
      | Error err ->
          Alcotest.fail (Printf.sprintf "Parse failed for %S: %s" src err.message)
      | Ok ast_prog -> (
          match Desugar.lower ast_prog with
          | Error err ->
              Alcotest.fail (Printf.sprintf "Desugar failed for %S: %s" src err.message)
          | Ok core_prog ->
              assert_program_provenance core_prog;
              Alcotest.(check string)
                "File name retained" ast_prog.span.file core_prog.span.file;
              Alcotest.(check int)
                "Start line retained" ast_prog.span.start_line core_prog.span.start_line;
              Alcotest.(check int)
                "End line retained" ast_prog.span.end_line core_prog.span.end_line))
    samples

let test_currying_span_lineage () =
  let src = "fun x y z -> x + y + z;" in
  match Parse_facade.parse_string ~file:"curry.vesper" src with
  | Error err -> Alcotest.fail err.message
  | Ok ast_prog -> (
      match Desugar.lower ast_prog with
      | Error err -> Alcotest.fail err.message
      | Ok core_prog -> (
          match core_prog.decls with
          | [ { desc = ExprDecl e; _ } ] -> (
              assert_expr_provenance e;
              (* Verify that nested lambdas all retain valid spans *)
              match e.desc with
              | Fun { param = "x"; body = l2 } -> (
                  assert_expr_provenance l2;
                  match l2.desc with
                  | Fun { param = "y"; body = l3 } -> (
                      assert_expr_provenance l3;
                      match l3.desc with
                      | Fun { param = "z"; _ } -> ()
                      | _ -> Alcotest.fail "Expected innermost fun z")
                  | _ -> Alcotest.fail "Expected fun y")
              | _ -> Alcotest.fail "Expected outermost fun x")
          | _ -> Alcotest.fail "Expected single ExprDecl"))

let test_compound_operator_span_lineage () =
  let src = "a && b || c;" in
  match Parse_facade.parse_string ~file:"compound.vesper" src with
  | Error err -> Alcotest.fail err.message
  | Ok ast_prog -> (
      match Desugar.lower ast_prog with
      | Error err -> Alcotest.fail err.message
      | Ok core_prog -> (
          assert_program_provenance core_prog;
          match core_prog.decls with
          | [ { desc = ExprDecl e; _ } ] ->
              (* e desugars to If for || *)
              assert_expr_provenance e;
              (match e.desc with
              | If { cond; then_branch; else_branch } ->
                  assert_expr_provenance cond;
                  assert_expr_provenance then_branch;
                  assert_expr_provenance else_branch;
                  Alcotest.(check bool)
                    "then_branch for || is true literal" true
                    (match then_branch.desc with Lit (Bool true) -> true | _ -> false)
              | _ -> Alcotest.fail "Expected If node from || desugaring");
              Alcotest.(check bool) "Expr span valid" true (span_is_valid e.span)
          | _ -> Alcotest.fail "Expected single ExprDecl"))

let test_fuzz_1000_provenance () =
  Random.init 42;
  for i = 1 to 1000 do
    let ast_prog = Test_law2_roundtrip.gen_program 3 in
    match Desugar.lower ast_prog with
    | Error err ->
        Alcotest.fail
          (Printf.sprintf "Desugar failed on fuzz iteration %d: %s" i err.message)
    | Ok core_prog ->
        assert_program_provenance core_prog;
        Alcotest.(check bool)
          "Program span matches input AST span" true
          (ast_prog.span = core_prog.span)
  done

let tests =
  [
    Alcotest.test_case "Handcrafted programs retain 100% span provenance" `Quick
      test_handcrafted_provenance;
    Alcotest.test_case "Curried lambda chain preserves span lineage" `Quick
      test_currying_span_lineage;
    Alcotest.test_case "Compound operators synthesize valid derived spans" `Quick
      test_compound_operator_span_lineage;
    Alcotest.test_case "1,000 random AST lowerings retain valid provenance (Law 3)" `Quick
      test_fuzz_1000_provenance;
  ]
