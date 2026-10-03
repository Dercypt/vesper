open Vesper

let run_code ?fuel ?env src =
  match Parse_facade.parse_string ~file:"<test>" src with
  | Error diag -> Error diag.message
  | Ok ast -> (
      match Desugar.lower ast with
      | Error err -> Error err.message
      | Ok core -> (
          match Eval.eval_program ?fuel ?env core with
          | Ok v -> Ok v
          | Error err -> Error err.message))

let check_int name expected src =
  match run_code src with
  | Ok (Value.VInt n) -> Alcotest.(check int) name expected n
  | Ok other ->
      Alcotest.fail
        (Printf.sprintf "%s: expected int %d, got %s" name expected
           (Value.to_string other))
  | Error err -> Alcotest.fail (Printf.sprintf "%s failed: %s" name err)

let check_bool name expected src =
  match run_code src with
  | Ok (Value.VBool b) -> Alcotest.(check bool) name expected b
  | Ok other ->
      Alcotest.fail
        (Printf.sprintf "%s: expected bool %b, got %s" name expected
           (Value.to_string other))
  | Error err -> Alcotest.fail (Printf.sprintf "%s failed: %s" name err)

let check_string name expected src =
  match run_code src with
  | Ok (Value.VString s) -> Alcotest.(check string) name expected s
  | Ok other ->
      Alcotest.fail
        (Printf.sprintf "%s: expected string %S, got %s" name expected
           (Value.to_string other))
  | Error err -> Alcotest.fail (Printf.sprintf "%s failed: %s" name err)

let check_error name expected_sub src =
  match run_code src with
  | Error err ->
      Alcotest.(check bool)
        (Printf.sprintf "%s: contains %S" name expected_sub)
        true
        (let len_sub = String.length expected_sub in
         let len_err = String.length err in
         let found = ref false in
         for i = 0 to len_err - len_sub do
           if String.sub err i len_sub = expected_sub then found := true
         done;
         !found)
  | Ok v ->
      Alcotest.fail
        (Printf.sprintf "%s: expected error containing %S, but got value %s" name
           expected_sub (Value.to_string v))

let test_arithmetic () =
  check_int "add" 42 "40 + 2;";
  check_int "sub" 38 "40 - 2;";
  check_int "mul" 120 "10 * 12;";
  check_int "div" 5 "20 / 4;";
  check_int "mod" 2 "17 % 5;";
  check_int "neg" (-15) "-15;";
  check_int "precedence" 14 "2 + 3 * 4;";
  check_int "parens" 20 "(2 + 3) * 4;";
  check_int "complex" 23 "5 * 5 - 4 / 2;"

let test_booleans_and_comparisons () =
  check_bool "true" true "true;";
  check_bool "false" false "false;";
  check_bool "not true" false "!true;";
  check_bool "not false" true "!false;";
  check_bool "eq int" true "42 == 42;";
  check_bool "neq int" true "42 != 0;";
  check_bool "lt" true "1 < 2;";
  check_bool "le" true "2 <= 2;";
  check_bool "gt" true "5 > 3;";
  check_bool "ge" true "5 >= 5;";
  check_bool "and true" true "true && true;";
  check_bool "and false" false "true && false;";
  check_bool "or true" true "false || true;";
  check_bool "or false" false "false || false;";
  check_bool "short circuit and" false "false && (1 / 0 == 0);";
  check_bool "short circuit or" true "true || (1 / 0 == 0);"

let test_strings () =
  check_string "literal" "hello" "\"hello\";";
  check_string "concat" "hello world" "\"hello \" + \"world\";";
  check_bool "string eq" true "\"vesper\" == \"vesper\";";
  check_bool "string neq" true "\"a\" != \"b\";";
  check_bool "string lt" true "\"apple\" < \"banana\";"

let test_scope_and_shadowing () =
  check_int "simple let" 10 "let x = 10; x;";
  check_int "let in expression" 15 "let x = 5 in x + 10;";
  check_int "shadowing in let body" 10 "let x = 1; let y = let x = 10 in x; y;";
  check_int "outer variable preserved after shadow" 11
    "let x = 1; let y = let x = 10 in x; x + y;";
  check_int "nested shadowing" 4 "let x = 1; let x = x + 1; let x = x * 2; x;";
  check_int "declaration sequence" 100 "let a = 10; let b = 20; let c = 70; a + b + c;"

let test_closures () =
  check_int "simple fun" 42 "let f = fun x -> x * 2; f 21;";
  check_int "curried fun" 30 "let add = fun x -> fun y -> x + y; add 10 20;";
  check_int "ast multi-arg fun" 30 "let add x y = x + y; add 10 20;";
  check_int "lexical environment capture" 15
    "let make_adder = fun x -> fun y -> x + y; let add5 = make_adder 5; let x = 999; \
     add5 10;";
  check_int "higher order function" 100
    "let apply = fun f -> fun x -> f x; let double = fun x -> x * 2; apply double 50;";
  check_int "function composition" 22
    "let compose = fun f -> fun g -> fun x -> f (g x); let mul2 = fun x -> x * 2; let \
     add1 = fun x -> x + 1; compose mul2 add1 10;"

let test_recursion () =
  check_int "factorial 5" 120
    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 5;";
  check_int "fibonacci 10" 55
    "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2); fib 10;";
  check_int "curried recursive function" 55
    "let rec sum_acc n acc = if n <= 0 then acc else sum_acc (n - 1) (acc + n); sum_acc \
     10 0;"

let test_fuel_and_termination () =
  (* Infinite recursion should hit fuel limit and return Execution budget exhausted *)
  check_error "infinite recursion hits budget" "Execution budget exhausted"
    "let rec loop n = loop (n + 1); loop 0;";
  (* With tiny fuel, even small computation exhausts budget *)
  let src = "let rec loop n = if n <= 0 then 0 else loop (n - 1); loop 10;" in
  (match run_code ~fuel:2 src with
  | Error msg -> Alcotest.(check string) "Exhausted" "Execution budget exhausted" msg
  | Ok _ -> Alcotest.fail "Expected fuel exhaustion");
  (* With sufficient fuel, it succeeds *)
  match run_code ~fuel:50 src with
  | Ok (Value.VInt 0) -> ()
  | _ -> Alcotest.fail "Expected Ok 0 with sufficient fuel"

let test_runtime_errors () =
  check_error "division by zero" "Division by zero" "10 / 0;";
  check_error "modulo by zero" "Division by zero" "10 % 0;";
  check_error "unbound variable" "Unbound variable 'unknown'" "unknown + 1;";
  check_error "type mismatch in addition" "Type error" "1 + true;";
  check_error "type mismatch in condition" "Type error" "if 42 then 1 else 2;";
  check_error "calling non-function" "Type error" "let x = 42; x 10;"

let () =
  let open Alcotest in
  run "Vesper Pure Evaluator Unit Tests"
    [
      ("Arithmetic & Expressions", [ test_case "Basic arithmetic" `Quick test_arithmetic ]);
      ( "Booleans & Comparisons",
        [ test_case "Booleans and logic" `Quick test_booleans_and_comparisons ] );
      ("Strings", [ test_case "String operations" `Quick test_strings ]);
      ( "Scoping & Shadowing",
        [ test_case "Lexical scope" `Quick test_scope_and_shadowing ] );
      ("Closures", [ test_case "Closures and currying" `Quick test_closures ]);
      ("Recursion", [ test_case "Recursive functions" `Quick test_recursion ]);
      ( "Fuel & Safety",
        [ test_case "Execution budget & fuel" `Quick test_fuel_and_termination ] );
      ("Runtime Errors", [ test_case "Type & zero errors" `Quick test_runtime_errors ]);
    ]
