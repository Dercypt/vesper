open Vesper
open Vesper.Pattern

let test_span : Ast.span =
  match
    Ast.create_span ~file:"test_patterns.vesper" ~start_line:1 ~start_col:1 ~end_line:1
      ~end_col:10
  with
  | Ok s -> s
  | Error _ -> Ast.dummy_span

let test_ast_and_constructors () =
  let w = wildcard ~span:test_span in
  let v = var ~span:test_span "x" in
  let lit_i = int ~span:test_span 42 in
  let lit_b = bool ~span:test_span true in
  let lit_s = string ~span:test_span "hello" in
  let ctor = construct ~span:test_span "Some" [ v ] in
  let tup = tuple ~span:test_span [ v; lit_i ] in
  let rec_pat = record ~span:test_span [ ("name", lit_s); ("val", lit_i) ] in
  let or_p = or_pat ~span:test_span (int ~span:test_span 1) (int ~span:test_span 2) in
  let aliased = alias ~span:test_span (var ~span:test_span "y") "z" in

  Alcotest.(check bool) "Wildcard equal" true (equal w (wildcard ~span:Ast.dummy_span));
  Alcotest.(check bool) "Var equal" true (equal v (var ~span:Ast.dummy_span "x"));
  Alcotest.(check bool) "Lit equal" true (equal lit_i (int ~span:Ast.dummy_span 42));
  Alcotest.(check bool) "Bool equal" true (equal lit_b (bool ~span:Ast.dummy_span true));
  Alcotest.(check bool)
    "Ctor equal" true
    (equal ctor (construct ~span:Ast.dummy_span "Some" [ var ~span:Ast.dummy_span "x" ]));
  Alcotest.(check bool)
    "Tuple equal" true
    (equal tup
       (tuple ~span:Ast.dummy_span
          [ var ~span:Ast.dummy_span "x"; int ~span:Ast.dummy_span 42 ]));
  Alcotest.(check bool)
    "Record equal" true
    (equal rec_pat
       (record ~span:Ast.dummy_span
          [
            ("name", string ~span:Ast.dummy_span "hello");
            ("val", int ~span:Ast.dummy_span 42);
          ]));
  Alcotest.(check bool)
    "Or equal" true
    (equal or_p
       (or_pat ~span:Ast.dummy_span (int ~span:Ast.dummy_span 1)
          (int ~span:Ast.dummy_span 2)));
  Alcotest.(check bool)
    "Alias equal" true
    (equal aliased (alias ~span:Ast.dummy_span (var ~span:Ast.dummy_span "y") "z"));

  Alcotest.(check (list string)) "Bound vars in tuple" [ "x" ] (bound_vars tup);
  Alcotest.(check (list string)) "Bound vars in alias" [ "z"; "y" ] (bound_vars aliased);
  Alcotest.(check (list string))
    "Bound vars in record" [ "x" ]
    (bound_vars (record ~span:test_span [ ("a", var ~span:test_span "x") ]));
  Alcotest.(check string) "String formatting" "Some(x)" (to_string ctor)

let test_validation_and_laws () =
  let valid_pat =
    construct ~span:test_span "Cons" [ var ~span:test_span "h"; wildcard ~span:test_span ]
  in
  Alcotest.(check bool) "Valid pattern validates" true (Result.is_ok (validate valid_pat));

  let invalid_span = { test_span with Ast.start_line = 10; Ast.end_line = 2 } in
  let bad_span_pat = var ~span:invalid_span "x" in
  Alcotest.(check bool)
    "Invalid span fails validation (Law 3)" true
    (Result.is_error (validate bad_span_pat));

  let empty_var_pat = var ~span:test_span "" in
  Alcotest.(check bool)
    "Empty variable fails validation" true
    (Result.is_error (validate empty_var_pat));

  let mismatched_or =
    or_pat ~span:test_span (var ~span:test_span "a") (var ~span:test_span "b")
  in
  Alcotest.(check bool)
    "Mismatched or-pattern bindings fail" true
    (Result.is_error (validate mismatched_or));

  let valid_or =
    or_pat ~span:test_span (var ~span:test_span "a") (var ~span:test_span "a")
  in
  Alcotest.(check bool)
    "Matching or-pattern succeeds" true
    (Result.is_ok (validate valid_or));

  let td = make_type_decl ~name:"color" [ ("Red", []); ("Green", []); ("Blue", []) ] in
  Alcotest.(check bool) "Type decl valid" true (Result.is_ok (validate_type_decl td));

  let rd = make_record_decl ~name:"point" [ ("x", Types.TyInt); ("y", Types.TyInt) ] in
  Alcotest.(check bool) "Record decl valid" true (Result.is_ok (validate_record_decl rd))

let test_maranget_exhaustiveness () =
  let sig_env = standard_sig_env in

  (* Option type: [Some x; None] is exhaustive *)
  let opt_exhaustive =
    [
      construct ~span:test_span "Some" [ var ~span:test_span "x" ];
      construct ~span:test_span "None" [];
    ]
  in
  let res1 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"option"
      opt_exhaustive
  in
  Alcotest.(check bool) "Option exhaustive is true" true res1.is_exhaustive;
  Alcotest.(check bool)
    "Option exhaustive has no missing witness" true (res1.missing_witness = None);

  (* Option type: [Some x] is non-exhaustive; missing is None *)
  let opt_missing = [ construct ~span:test_span "Some" [ var ~span:test_span "x" ] ] in
  let res2 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"option"
      opt_missing
  in
  Alcotest.(check bool) "Option incomplete is false" false res2.is_exhaustive;
  Alcotest.(check bool)
    "Witness is None" true
    (Option.map to_string res2.missing_witness = Some "None");

  (* Result type: [Ok x; Error e] is exhaustive *)
  let res_exhaustive =
    [
      construct ~span:test_span "Ok" [ var ~span:test_span "v" ];
      construct ~span:test_span "Error" [ var ~span:test_span "err" ];
    ]
  in
  let res3 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"result"
      res_exhaustive
  in
  Alcotest.(check bool) "Result exhaustive" true res3.is_exhaustive;

  (* Result type: [Ok x] is non-exhaustive *)
  let res_incomplete = [ construct ~span:test_span "Ok" [ var ~span:test_span "v" ] ] in
  let res4 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"result"
      res_incomplete
  in
  Alcotest.(check bool) "Result incomplete" false res4.is_exhaustive;
  Alcotest.(check bool)
    "Witness is Error(_)" true
    (Option.map to_string res4.missing_witness = Some "Error(_)");

  (* Boolean: [true; false] is exhaustive *)
  let bool_exhaustive = [ bool ~span:test_span true; bool ~span:test_span false ] in
  let res5 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"bool"
      bool_exhaustive
  in
  Alcotest.(check bool) "Bool exhaustive" true res5.is_exhaustive;

  (* Boolean: [true] is non-exhaustive *)
  let bool_missing = [ bool ~span:test_span true ] in
  let res6 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"bool"
      bool_missing
  in
  Alcotest.(check bool) "Bool incomplete" false res6.is_exhaustive;
  Alcotest.(check bool)
    "Witness is false" true
    (Option.map to_string res6.missing_witness = Some "false");

  (* List: [Nil; Cons(h, t)] is exhaustive *)
  let list_exhaustive =
    [
      construct ~span:test_span "Nil" [];
      construct ~span:test_span "Cons"
        [ var ~span:test_span "h"; var ~span:test_span "t" ];
    ]
  in
  let res7 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"list"
      list_exhaustive
  in
  Alcotest.(check bool) "List exhaustive" true res7.is_exhaustive;

  (* List: [Nil] is non-exhaustive *)
  let list_missing = [ construct ~span:test_span "Nil" [] ] in
  let res8 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"list"
      list_missing
  in
  Alcotest.(check bool) "List incomplete" false res8.is_exhaustive;
  Alcotest.(check bool)
    "Witness is Cons(_, _)" true
    (Option.map to_string res8.missing_witness = Some "Cons(_, _)");

  (* Nested Option: [Some (Some x); Some None; None] is exhaustive *)
  let nested_exhaustive =
    [
      construct ~span:test_span "Some"
        [ construct ~span:test_span "Some" [ var ~span:test_span "x" ] ];
      construct ~span:test_span "Some" [ construct ~span:test_span "None" [] ];
      construct ~span:test_span "None" [];
    ]
  in
  let res9 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"option"
      nested_exhaustive
  in
  Alcotest.(check bool) "Nested option exhaustive" true res9.is_exhaustive

let test_maranget_redundancy () =
  let sig_env = standard_sig_env in

  (* Redundant wildcard: [x; y] *)
  let redundant_wildcards = [ var ~span:test_span "x"; var ~span:test_span "y" ] in
  let res1 = Exhaustiveness.check_patterns ~sig_env ~span:test_span redundant_wildcards in
  Alcotest.(check (list int)) "Second wildcard redundant" [ 1 ] res1.redundant_indices;
  Alcotest.(check int) "One redundant diagnostic emitted" 1 (List.length res1.diagnostics);

  (* Redundant bool: [true; false; true] *)
  let redundant_bool =
    [ bool ~span:test_span true; bool ~span:test_span false; bool ~span:test_span true ]
  in
  let res2 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"bool"
      redundant_bool
  in
  Alcotest.(check (list int)) "Third bool clause redundant" [ 2 ] res2.redundant_indices;

  (* Redundant constructor: [Some x; None; Some y] *)
  let redundant_opt =
    [
      construct ~span:test_span "Some" [ var ~span:test_span "x" ];
      construct ~span:test_span "None" [];
      construct ~span:test_span "Some" [ var ~span:test_span "y" ];
    ]
  in
  let res3 =
    Exhaustiveness.check_patterns ~sig_env ~span:test_span ~expected_type:"option"
      redundant_opt
  in
  Alcotest.(check (list int)) "Second Some redundant" [ 2 ] res3.redundant_indices;

  (* Non-redundant with or-patterns: [1 | 2; 3; 4] *)
  let non_redundant_or =
    [
      or_pat ~span:test_span (int ~span:test_span 1) (int ~span:test_span 2);
      int ~span:test_span 3;
      int ~span:test_span 4;
    ]
  in
  let res4 = Exhaustiveness.check_patterns ~sig_env ~span:test_span non_redundant_or in
  Alcotest.(check (list int)) "No redundancy" [] res4.redundant_indices

let test_decision_tree_compilation () =
  let sig_env = standard_sig_env in
  let target = Core_ir.make_expr ~span:test_span (Core_ir.Var "input") in
  let dummy_body1 = Core_ir.make_expr ~span:test_span (Core_ir.Lit (Ast.Int 1)) in
  let dummy_body2 = Core_ir.make_expr ~span:test_span (Core_ir.Lit (Ast.Int 2)) in

  let clauses =
    [
      make_clause ~span:test_span
        (construct ~span:test_span "Some" [ var ~span:test_span "v" ])
        dummy_body1;
      make_clause ~span:test_span (construct ~span:test_span "None" []) dummy_body2;
    ]
  in

  let tree_res =
    Pat_compile.compile ~sig_env ~span:test_span ~target_expr:target clauses
  in
  Alcotest.(check bool) "Compilation succeeds" true (Result.is_ok tree_res);
  match tree_res with
  | Ok tree ->
      Alcotest.(check bool)
        "Tree string representation non-empty" true
        (String.length (Pat_compile.to_string tree) > 0)
  | Error err -> Alcotest.fail err.message

let test_decision_tree_lowering_to_core () =
  let span = test_span in
  let b1 = Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 10)) in
  let b2 = Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 20)) in

  let bool_tree =
    Pat_compile.SwitchBool
      {
        target = Pat_compile.Here;
        span;
        true_branch =
          Pat_compile.Leaf { action_id = 0; span; bindings = []; guard = None; body = b1 };
        false_branch =
          Pat_compile.Leaf { action_id = 1; span; bindings = []; guard = None; body = b2 };
      }
  in
  let lowered = Pat_compile.lower_to_core ~span ~target_var:"b" bool_tree in
  Alcotest.(check bool) "Lowering bool switch succeeds" true (Result.is_ok lowered);
  match lowered with
  | Ok core_expr -> (
      Alcotest.(check bool)
        "Span is valid on lowered Core IR (Law 3)" true
        (Ast.span_is_valid core_expr.span);
      match core_expr.desc with
      | Core_ir.If { then_branch; else_branch; _ } ->
          Alcotest.(check bool)
            "Then branch equals b1" true
            (Core_ir.expr_equal then_branch b1);
          Alcotest.(check bool)
            "Else branch equals b2" true
            (Core_ir.expr_equal else_branch b2)
      | _ -> Alcotest.fail "Expected If node in lowered Core IR")
  | Error err -> Alcotest.fail err.message

let test_deterministic_evaluation () =
  let sig_env = standard_sig_env in
  let span = test_span in
  let env = Env.empty in

  (* Test 1: Literal matching *)
  let lit_clauses =
    [
      make_clause ~span (int ~span 0)
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.String "zero")));
      make_clause ~span (int ~span 1)
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.String "one")));
      make_clause ~span (wildcard ~span)
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.String "other")));
    ]
  in
  let r0 = Pat_compile.eval_match ~sig_env ~span env (Value.int 0) lit_clauses in
  Alcotest.(check bool) "Match 0 evaluates to zero" true (r0 = Ok (Value.string "zero"));
  let r1 = Pat_compile.eval_match ~sig_env ~span env (Value.int 1) lit_clauses in
  Alcotest.(check bool) "Match 1 evaluates to one" true (r1 = Ok (Value.string "one"));
  let r99 = Pat_compile.eval_match ~sig_env ~span env (Value.int 99) lit_clauses in
  Alcotest.(check bool)
    "Match 99 evaluates to other" true
    (r99 = Ok (Value.string "other"));

  (* Test 2: Option matching and variable extraction *)
  let opt_clauses =
    [
      make_clause ~span
        (construct ~span "Some" [ var ~span "x" ])
        (Core_ir.make_expr ~span (Core_ir.Var "x"));
      make_clause ~span (construct ~span "None" [])
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int (-1))));
    ]
  in
  let r_some =
    Pat_compile.eval_match ~sig_env ~span env
      (Value.construct "Some" [ Value.int 42 ])
      opt_clauses
  in
  Alcotest.(check bool) "Some 42 extracts 42" true (r_some = Ok (Value.int 42));
  let r_none =
    Pat_compile.eval_match ~sig_env ~span env (Value.construct "None" []) opt_clauses
  in
  Alcotest.(check bool) "None extracts -1" true (r_none = Ok (Value.int (-1)));

  (* Test 3: Nested list matching *)
  let list_clauses =
    [
      make_clause ~span (construct ~span "Nil" [])
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 0)));
      make_clause ~span
        (construct ~span "Cons"
           [ var ~span "h"; construct ~span "Cons" [ var ~span "h2"; wildcard ~span ] ])
        (Core_ir.make_expr ~span
           (Core_ir.PrimOp
              {
                op = Core_ir.Add;
                args =
                  [
                    Core_ir.make_expr ~span (Core_ir.Var "h");
                    Core_ir.make_expr ~span (Core_ir.Var "h2");
                  ];
              }));
      make_clause ~span
        (construct ~span "Cons" [ var ~span "h"; wildcard ~span ])
        (Core_ir.make_expr ~span (Core_ir.Var "h"));
    ]
  in
  let list_val =
    Value.construct "Cons"
      [ Value.int 10; Value.construct "Cons" [ Value.int 20; Value.construct "Nil" [] ] ]
  in
  let r_list = Pat_compile.eval_match ~sig_env ~span env list_val list_clauses in
  Alcotest.(check bool) "Nested list sum 10 + 20 = 30" true (r_list = Ok (Value.int 30));

  (* Test 4: Record matching *)
  let rec_val = Value.record [ ("x", Value.int 100); ("y", Value.int 200) ] in
  let rec_clauses =
    [
      make_clause ~span
        (record ~span [ ("x", var ~span "a"); ("y", var ~span "b") ])
        (Core_ir.make_expr ~span
           (Core_ir.PrimOp
              {
                op = Core_ir.Add;
                args =
                  [
                    Core_ir.make_expr ~span (Core_ir.Var "a");
                    Core_ir.make_expr ~span (Core_ir.Var "b");
                  ];
              }));
    ]
  in
  let r_rec = Pat_compile.eval_match ~sig_env ~span env rec_val rec_clauses in
  Alcotest.(check bool)
    "Record destructuring 100 + 200 = 300" true
    (r_rec = Ok (Value.int 300));

  (* Test 5: Guard matching *)
  let guard_expr =
    Core_ir.make_expr ~span
      (Core_ir.PrimOp
         {
           op = Core_ir.Gt;
           args =
             [
               Core_ir.make_expr ~span (Core_ir.Var "n");
               Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 0));
             ];
         })
  in
  let guarded_clauses =
    [
      make_clause ~span ~guard:guard_expr (var ~span "n")
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.String "positive")));
      make_clause ~span (wildcard ~span)
        (Core_ir.make_expr ~span (Core_ir.Lit (Ast.String "non-positive")));
    ]
  in
  let r_pos = Pat_compile.eval_match ~sig_env ~span env (Value.int 5) guarded_clauses in
  Alcotest.(check bool)
    "Guarded match 5 is positive" true
    (r_pos = Ok (Value.string "positive"));
  let r_neg =
    Pat_compile.eval_match ~sig_env ~span env (Value.int (-3)) guarded_clauses
  in
  Alcotest.(check bool)
    "Guarded match -3 is non-positive" true
    (r_neg = Ok (Value.string "non-positive"));

  (* Test 6: Determinism verification across 10 iterations (Law 5) *)
  for _ = 1 to 10 do
    let r_iter = Pat_compile.eval_match ~sig_env ~span env list_val list_clauses in
    Alcotest.(check bool)
      "Bit-for-bit determinism across runs" true
      (r_iter = Ok (Value.int 30))
  done

let test_law1_host_isolation () =
  let sig_env = standard_sig_env in
  let span = test_span in
  let env = Env.empty in

  (* Incomplete match evaluated via eval_match returns Error rather than crashing host *)
  let incomplete_clauses =
    [
      make_clause ~span
        (construct ~span "Some" [ var ~span "x" ])
        (Core_ir.make_expr ~span (Core_ir.Var "x"));
    ]
  in
  let r =
    Pat_compile.eval_match ~sig_env ~span env (Value.construct "None" [])
      incomplete_clauses
  in
  Alcotest.(check bool)
    "Incomplete match returns Result.Error gracefully (Law 1)" true (Result.is_error r);
  match r with
  | Error err ->
      Alcotest.(check bool) "Error span is valid" true (Ast.span_is_valid err.span);
      Alcotest.(check bool) "Error message non-empty" true (String.length err.message > 0)
  | Ok _ -> Alcotest.fail "Expected error on incomplete match"

let () =
  let open Alcotest in
  run "Vesper Pattern Matching & ADT Unit Tests"
    [
      ( "Pattern AST & Constructors",
        [
          test_case "Smart constructors and representations" `Quick
            test_ast_and_constructors;
        ] );
      ( "Pattern Validation & Laws",
        [ test_case "Provenance validation and Law 3" `Quick test_validation_and_laws ] );
      ( "Maranget Exhaustiveness",
        [
          test_case "Sum types, options, results, lists exhaustiveness" `Quick
            test_maranget_exhaustiveness;
        ] );
      ( "Maranget Redundancy",
        [
          test_case "Dead-code diagnostics and redundant clauses" `Quick
            test_maranget_redundancy;
        ] );
      ( "Decision Tree Compilation",
        [
          test_case "Matrix decomposition into decision tree" `Quick
            test_decision_tree_compilation;
        ] );
      ( "Core IR Lowering",
        [
          test_case "Lowering decision trees to Core IR conditionals" `Quick
            test_decision_tree_lowering_to_core;
        ] );
      ( "Deterministic Evaluation",
        [
          test_case "Runtime matching, closures, recursion, and Law 5" `Quick
            test_deterministic_evaluation;
        ] );
      ( "Host Isolation & Safety",
        [
          test_case "Graceful handling of unhandled matches (Law 1)" `Quick
            test_law1_host_isolation;
        ] );
    ]
