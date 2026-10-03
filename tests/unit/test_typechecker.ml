open Vesper

let parse_and_typecheck src =
  match Parse_facade.parse_string ~file:"<test>" src with
  | Error diag -> Error diag.message
  | Ok ast -> (
      match Typecheck.typecheck_program ast with
      | Ok typed_prog ->
          let last_ty =
            match List.rev typed_prog.decls with
            | [] -> Types.TyUnit
            | d :: _ -> (
                match d.desc with
                | Typecheck.TypedExprDecl e -> e.ty
                | Typecheck.TypedLetDecl ld -> (
                    match ld.scheme with Types.Forall (_, ty) -> ty))
          in
          Ok (Types.normalize_vars last_ty)
      | Error diags ->
          let msg =
            String.concat "\n" (List.map (fun (d : Diagnostic.t) -> d.message) diags)
          in
          Error msg)

let check_type name expected_str src =
  match parse_and_typecheck src with
  | Ok ty ->
      let actual_str = Types.to_string ty in
      Alcotest.(check string) name expected_str actual_str
  | Error err -> Alcotest.fail (Printf.sprintf "%s failed with error: %s" name err)

let check_type_err name expected_sub src =
  match parse_and_typecheck src with
  | Error err ->
      let len_sub = String.length expected_sub in
      let len_err = String.length err in
      let found = ref false in
      for i = 0 to len_err - len_sub do
        if String.sub err i len_sub = expected_sub then found := true
      done;
      Alcotest.(check bool)
        (Printf.sprintf "%s: contains %S" name expected_sub)
        true !found
  | Ok ty ->
      Alcotest.fail
        (Printf.sprintf "%s: expected type error containing %S, but got type %s" name
           expected_sub (Types.to_string ty))

let test_types_and_unify () =
  (* Types.to_string *)
  Alcotest.(check string) "int" "int" (Types.to_string Types.TyInt);
  Alcotest.(check string) "bool" "bool" (Types.to_string Types.TyBool);
  Alcotest.(check string) "string" "string" (Types.to_string Types.TyString);
  Alcotest.(check string) "unit" "unit" (Types.to_string Types.TyUnit);
  Alcotest.(check string) "var" "'a" (Types.to_string (Types.TyVar 0));
  Alcotest.(check string) "var high" "'t30" (Types.to_string (Types.TyVar 30));
  Alcotest.(check string)
    "arrow" "int -> bool"
    (Types.to_string (Types.TyArrow (Types.TyInt, Types.TyBool)));
  Alcotest.(check string)
    "arrow assoc right" "int -> bool -> string"
    (Types.to_string
       (Types.TyArrow (Types.TyInt, Types.TyArrow (Types.TyBool, Types.TyString))));
  Alcotest.(check string)
    "arrow nested left" "(int -> bool) -> string"
    (Types.to_string
       (Types.TyArrow (Types.TyArrow (Types.TyInt, Types.TyBool), Types.TyString)));

  (* Types.equal & scheme_equal *)
  Alcotest.(check bool) "equal" true (Types.equal Types.TyInt Types.TyInt);
  Alcotest.(check bool) "not equal" false (Types.equal Types.TyInt Types.TyBool);
  let s1 = Types.Forall ([ 0 ], Types.TyArrow (Types.TyVar 0, Types.TyVar 0)) in
  let s2 = Types.Forall ([ 5 ], Types.TyArrow (Types.TyVar 5, Types.TyVar 5)) in
  Alcotest.(check bool) "scheme alpha equal" true (Types.scheme_equal s1 s2);

  (* Unify *)
  let span = Ast.dummy_span in
  (match Unify.unify ~span Types.TyInt Types.TyInt with
  | Ok s -> Alcotest.(check bool) "unify int empty" true (Types.IntMap.is_empty s)
  | Error _ -> Alcotest.fail "Unify int failed");
  (match Unify.unify ~span (Types.TyVar 0) Types.TyInt with
  | Ok s -> (
      match Types.IntMap.find_opt 0 s with
      | Some Types.TyInt -> ()
      | _ -> Alcotest.fail "Expected var 0 to map to TyInt")
  | Error _ -> Alcotest.fail "Unify var 0 with int failed");
  (match Unify.unify ~span Types.TyInt Types.TyBool with
  | Ok _ -> Alcotest.fail "Expected type mismatch between int and bool"
  | Error err ->
      Alcotest.(check string)
        "err message" "Type mismatch: expected 'int', but got 'bool'" err.message);

  (* Occurs check *)
  let infinite_ty = Types.TyArrow (Types.TyVar 0, Types.TyInt) in
  Alcotest.(check bool)
    "occurs check detects cycle" true
    (Unify.occurs_check 0 infinite_ty);
  match Unify.unify ~span (Types.TyVar 0) infinite_ty with
  | Ok _ -> Alcotest.fail "Expected occurs check failure"
  | Error err ->
      let len_sub = String.length "Occurs check failed" in
      let found =
        String.length err.message >= len_sub
        && String.sub err.message 0 len_sub = "Occurs check failed"
      in
      Alcotest.(check bool) "occurs check error message" true found

let test_primitives_and_arithmetic () =
  check_type "int literal" "int" "42;";
  check_type "bool literal true" "bool" "true;";
  check_type "bool literal false" "bool" "false;";
  check_type "string literal" "string" "\"vesper\";";
  check_type "add int" "int" "40 + 2;";
  check_type "sub int" "int" "40 - 2;";
  check_type "mul int" "int" "6 * 7;";
  check_type "div int" "int" "84 / 2;";
  check_type "mod int" "int" "45 % 3;";
  check_type "neg int" "int" "-42;";
  check_type "string concat" "string" "\"hello \" + \"world\";";
  check_type "not bool" "bool" "!true;";
  check_type "and bool" "bool" "true && false;";
  check_type "or bool" "bool" "false || true;";
  check_type "eq int" "bool" "42 == 42;";
  check_type "neq string" "bool" "\"a\" != \"b\";";
  check_type "lt int" "bool" "1 < 2;";
  check_type "ge string" "bool" "\"z\" >= \"a\";";
  check_type "if expression" "int" "if true then 1 else 2;"

let test_functions_and_currying () =
  check_type "simple identity" "'a -> 'a" "fun x -> x;";
  check_type "increment" "int -> int" "fun x -> x + 1;";
  check_type "binary adder" "int -> int -> int" "fun x y -> x + y;";
  check_type "curried application" "int" "let add x y = x + y in add 1 2;";
  check_type "immediate application" "int" "(fun x -> x * 2) 21;";
  check_type "lambda capturing scope" "int -> int"
    "let factor = 10 in fun x -> x * factor;"

let test_higher_order_and_polymorphism () =
  (* Polymorphic identity used at multiple types *)
  check_type "poly identity multi use" "string"
    "let id = fun x -> x; let n = id 42; let b = id true; let s = id \"vesper\"; s;";

  (* Higher-order compose *)
  check_type "compose" "('c -> 'a) -> ('b -> 'c) -> 'b -> 'a" "fun f g x -> f (g x);";

  check_type "compose application" "int"
    "let compose f g x = f (g x); let double x = x * 2; let inc x = x + 1; compose \
     double inc 20;";

  (* Higher-order apply *)
  check_type "apply" "('b -> 'a) -> 'b -> 'a" "fun f x -> f x;";

  (* Higher-order twice *)
  check_type "twice" "('a -> 'a) -> 'a -> 'a" "fun f x -> f (f x);";

  check_type "twice application" "int"
    "let twice f x = f (f x); let add10 x = x + 10; twice add10 0;";

  (* Church numerals *)
  check_type "church zero" "'b -> 'a -> 'a" "fun f x -> x;";
  check_type "church succ" "(('c -> 'a) -> 'b -> 'c) -> ('c -> 'a) -> 'b -> 'a"
    "fun n f x -> f (n f x);";
  check_type "church numeral evaluation" "int"
    "let zero = fun f x -> x; let succ = fun n f x -> f (n f x); let one = succ zero; \
     let two = succ one; let to_int = fun n -> n (fun x -> x + 1) 0; to_int two;"

let test_recursion () =
  check_type "factorial" "int -> int"
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact;";
  check_type "factorial application" "int"
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 5;";
  check_type "fibonacci" "int -> int"
    "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2); fib;";
  check_type "gcd" "int -> int -> int"
    "let rec gcd a b = if b == 0 then a else gcd b (a % b); gcd;"

let parse_expr src =
  match Parse_facade.parse_string ~file:"<bidirectional_test>" src with
  | Ok { decls = [ { decl_desc = ExprDecl e; _ } ]; _ } -> e
  | _ -> Alcotest.fail ("Failed to parse expression: " ^ src)

let test_bidirectional_checking () =
  let env = Type_env.empty in

  (* Checking a lambda directly against an arrow type *)
  let expr1 = parse_expr "fun x -> x + 1;" in
  let target_ty = Types.TyArrow (Types.TyInt, Types.TyInt) in
  (match Typecheck.check env expr1 target_ty with
  | Ok _ -> ()
  | Error err -> Alcotest.fail ("Bidirectional check failed: " ^ err.message));

  (* Checking lambda against wrong arrow type fails cleanly *)
  let wrong_ty = Types.TyArrow (Types.TyString, Types.TyString) in
  (match Typecheck.check env expr1 wrong_ty with
  | Ok _ -> Alcotest.fail "Expected check against string -> string to fail"
  | Error _ -> ());

  (* Checking conditional against expected type *)
  let expr_if = parse_expr "if true then 10 else 20;" in
  (match Typecheck.check env expr_if Types.TyInt with
  | Ok _ -> ()
  | Error err -> Alcotest.fail ("Checking if against int failed: " ^ err.message));
  match Typecheck.check env expr_if Types.TyBool with
  | Ok _ -> Alcotest.fail "Expected checking if of ints against bool to fail"
  | Error _ -> ()

let test_negative_type_errors () =
  check_type_err "int + bool" "Type mismatch" "1 + true;";
  check_type_err "not int" "Type mismatch" "!42;";
  check_type_err "sub bool" "Type mismatch" "true - false;";
  check_type_err "if non-bool cond" "Type mismatch" "if 42 then 1 else 2;";
  check_type_err "if branch mismatch" "Type mismatch" "if true then 1 else \"hello\";";
  check_type_err "unbound variable" "Unbound identifier 'unknown_var'" "unknown_var + 1;";
  check_type_err "occurs check infinite type" "Occurs check failed"
    "let f = fun x -> x x in f;";
  check_type_err "calling non-function" "Type mismatch" "let x = 42; x 10;";
  check_type_err "function argument mismatch" "Type mismatch"
    "let inc = fun x -> x + 1; inc \"forty-two\";"

let test_diagnostics_and_spans () =
  (* Verify that error coordinates point to offending token span *)
  let src = "let x = 1 + true;\nx;" in
  match Parse_facade.parse_string ~file:"test_span.vesper" src with
  | Error _ -> Alcotest.fail "Parse failed"
  | Ok ast -> (
      match Typecheck.typecheck_program ast with
      | Ok _ -> Alcotest.fail "Expected type error"
      | Error diags ->
          Alcotest.(check int) "one diagnostic" 1 (List.length diags);
          let diag = List.hd diags in
          Alcotest.(check bool) "span is valid" true (Ast.span_is_valid diag.span);
          Alcotest.(check int) "start line 1" 1 diag.span.start_line;
          Alcotest.(check bool)
            "diagnostic message non-empty" true
            (String.length diag.message > 0))

let test_law1_host_isolation () =
  (* Adversarial invalid ASTs passed directly to Typecheck must never raise host exceptions *)
  let invalid_span =
    { Ast.file = "bogus"; start_line = 10; start_col = 5; end_line = 2; end_col = 0 }
  in
  let bogus_expr = { Ast.span = invalid_span; desc = Ast.Lit (Ast.Int 42) } in
  match Typecheck.infer Type_env.empty bogus_expr with
  | Ok _ -> Alcotest.fail "Expected Law 3 invalid span error"
  | Error err ->
      Alcotest.(check bool) "Invalid span detected" true (String.length err.message > 0)

let () =
  let open Alcotest in
  run "Vesper Static Typechecker Unit Tests"
    [
      ( "Types & Unification",
        [ test_case "Unification algebra" `Quick test_types_and_unify ] );
      ( "Primitives & Arithmetic",
        [ test_case "Basic type inference" `Quick test_primitives_and_arithmetic ] );
      ( "Functions & Currying",
        [ test_case "Arrow types & applications" `Quick test_functions_and_currying ] );
      ( "Higher-Order & Polymorphism",
        [
          test_case "Let-polymorphism & Church" `Quick test_higher_order_and_polymorphism;
        ] );
      ("Recursion", [ test_case "Recursive bindings" `Quick test_recursion ]);
      ( "Bidirectional Checking",
        [ test_case "Check mode verification" `Quick test_bidirectional_checking ] );
      ( "Negative Diagnostics",
        [ test_case "Type error reporting" `Quick test_negative_type_errors ] );
      ( "Diagnostic Spans",
        [ test_case "Source span accuracy" `Quick test_diagnostics_and_spans ] );
      ( "Law 1 Host Isolation",
        [ test_case "No host exceptions on bad input" `Quick test_law1_host_isolation ] );
    ]
