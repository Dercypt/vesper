open Vesper
module VM = Vm

let dummy_span = Ast.dummy_span

let test_opcode_operations () =
  let ops =
    [
      Opcode.Op_Const 0;
      Opcode.Op_Add;
      Opcode.Op_Sub;
      Opcode.Op_Mul;
      Opcode.Op_Div;
      Opcode.Op_Mod;
      Opcode.Op_Neg;
      Opcode.Op_Not;
      Opcode.Op_Eq;
      Opcode.Op_Neq;
      Opcode.Op_Lt;
      Opcode.Op_Le;
      Opcode.Op_Gt;
      Opcode.Op_Ge;
      Opcode.Op_GetLocal 1;
      Opcode.Op_SetLocal 1;
      Opcode.Op_GetGlobal "x";
      Opcode.Op_DefGlobal "x";
      Opcode.Op_SetGlobal "x";
      Opcode.Op_Jump 10;
      Opcode.Op_JumpIfFalse 5;
      Opcode.Op_Call 1;
      Opcode.Op_Return;
      Opcode.Op_Halt;
      Opcode.Op_Pop;
      Opcode.Op_Unit;
      Opcode.Op_Closure 0;
      Opcode.Op_TieRec "f";
    ]
  in
  List.iter
    (fun op ->
      Alcotest.(check bool) "equal to self" true (Opcode.equal op op);
      let str = Opcode.to_string op in
      Alcotest.(check bool) "non-empty string" true (String.length str > 0))
    ops;
  Alcotest.(check bool)
    "different opcodes not equal" false
    (Opcode.equal Opcode.Op_Add Opcode.Op_Sub)

let test_chunk_operations () =
  let chunk =
    Bytecode.create
      ~code:[| Opcode.Op_Const 0; Opcode.Op_Halt |]
      ~constants:[| Value.VInt 42 |] ~spans:[| dummy_span; dummy_span |] ()
  in
  Alcotest.(check int) "length is 2" 2 (Bytecode.length chunk);
  Alcotest.(check bool)
    "opcode 0 is Op_Const 0" true
    (Opcode.equal (Bytecode.get_opcode chunk 0) (Opcode.Op_Const 0));
  Alcotest.(check bool)
    "constant 0 is 42" true
    (Value.equal (Bytecode.get_constant chunk 0) (Value.VInt 42));
  Alcotest.(check bool) "equal self" true (Bytecode.equal chunk chunk);
  Alcotest.(check bool) "not equal empty" false (Bytecode.equal chunk Bytecode.empty)

let test_compiler_and_vm_basic () =
  let run_src src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag.message
    | Ok ast -> (
        match Compiler.compile ast with
        | Error err -> Error err.message
        | Ok chunk -> (
            match VM.run_outcome_ast_error chunk with
            | Ok out -> Ok out.value
            | Error err -> Error err.message))
  in
  (match run_src "10 + 20 * 2;" with
  | Ok (Value.VInt 50) -> ()
  | _ -> Alcotest.fail "Expected 50");
  (match run_src "let x = 15; let y = 25; x + y;" with
  | Ok (Value.VInt 40) -> ()
  | _ -> Alcotest.fail "Expected 40");
  (match run_src "let f x = x * x; f 6;" with
  | Ok (Value.VInt 36) -> ()
  | _ -> Alcotest.fail "Expected 36");
  (match run_src "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 5;" with
  | Ok (Value.VInt 120) -> ()
  | _ -> Alcotest.fail "Expected 120");
  let parse_and_compile src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> failwith diag.message
    | Ok ast -> (
        match Compiler.compile ast with
        | Error err -> failwith err.message
        | Ok chunk -> chunk)
  in
  let chunk_direct = parse_and_compile "21 * 2;" in
  (match VM.run chunk_direct with
  | Ok (Value.VInt 42) -> ()
  | _ -> Alcotest.fail "Expected 42 from VM.run");
  let chunk_env = parse_and_compile "x + y;" in
  let env = [ ("x", Value.VInt 10); ("y", Value.VInt 32) ] in
  match VM.run ~env chunk_env with
  | Ok (Value.VInt 42) -> ()
  | _ -> Alcotest.fail "Expected 42 from VM.run with env"

let test_vm_errors () =
  let run_src src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag.message
    | Ok ast -> (
        match Compiler.compile ast with
        | Error err -> Error err.message
        | Ok chunk -> (
            match VM.run_ast_error chunk with
            | Ok _ -> Ok ()
            | Error err -> Error err.message))
  in
  (match run_src "10 / 0;" with
  | Error msg -> Alcotest.(check string) "divzero error" "Division by zero" msg
  | Ok () -> Alcotest.fail "Expected divzero error");
  (match run_src "not_found + 1;" with
  | Error msg ->
      Alcotest.(check string) "unbound error" "Unbound variable 'not_found'" msg
  | Ok () -> Alcotest.fail "Expected unbound error");
  (match run_src "let x = 1; x 2;" with
  | Error msg ->
      Alcotest.(check bool) "type error" true (String.sub msg 0 10 = "Type error")
  | Ok () -> Alcotest.fail "Expected type error");
  let run_diag ?fuel src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag
    | Ok ast -> (
        match Compiler.compile ast with
        | Error err -> Error (Diagnostic.of_ast_error err)
        | Ok chunk -> VM.run ?fuel chunk)
  in
  (match run_diag "10 / 0;" with
  | Error diag ->
      Alcotest.(check bool)
        "divzero error code" true
        (Error_code.equal diag.code Error_code.E5001_division_by_zero);
      Alcotest.(check string) "divzero message" "Division by zero" diag.message
  | Ok _ -> Alcotest.fail "Expected divzero error");
  (match run_diag "10 % 0;" with
  | Error diag ->
      Alcotest.(check bool)
        "modzero error code" true
        (Error_code.equal diag.code Error_code.E5001_division_by_zero)
  | Ok _ -> Alcotest.fail "Expected modzero error");
  match run_diag ~fuel:0 "let x = 1;" with
  | Error diag ->
      Alcotest.(check bool)
        "fuel exhausted code" true
        (Error_code.equal diag.code Error_code.E5002_fuel_exhausted);
      Alcotest.(check string)
        "fuel exhausted message" "Execution budget exhausted" diag.message
  | Ok _ -> Alcotest.fail "Expected fuel exhausted error"

let test_static_closures () =
  let run_src src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag.message
    | Ok ast -> (
        match Compiler.compile ast with
        | Error err -> Error err.message
        | Ok chunk -> (
            match VM.run_outcome_ast_error chunk with
            | Ok out -> Ok out.value
            | Error err -> Error err.message))
  in
  (* Closure returned from outer function (higher-order return) *)
  (match
     run_src
       "let make_adder x = (let add_x y = x + y in add_x); let add10 = make_adder 10; \
        add10 5;"
   with
  | Ok (Value.VInt 15) -> ()
  | _ -> Alcotest.fail "Expected 15 from returned closure");
  (* Higher-order argument *)
  (match run_src "let apply f x = f x; let square x = x * x; apply square 7;" with
  | Ok (Value.VInt 49) -> ()
  | _ -> Alcotest.fail "Expected 49 from higher-order argument");
  (* Curried 3 arguments *)
  (match run_src "let add3 a b c = a + b + c; add3 10 20 30;" with
  | Ok (Value.VInt 60) -> ()
  | _ -> Alcotest.fail "Expected 60 from curried 3 args");
  (* Local recursive function *)
  (match run_src "let rec f n = if n <= 1 then 1 else n * f (n - 1); f 4;" with
  | Ok (Value.VInt 24) -> ()
  | _ -> Alcotest.fail "Expected 24 from recursion");
  (* Native VM closure execution returning Vm.value *)
  match Parse_facade.parse_string ~file:"<test>" "let f x = x + 1; f;" with
  | Error diag -> Alcotest.fail diag.message
  | Ok ast -> (
      match Compiler.compile ast with
      | Error err -> Alcotest.fail err.message
      | Ok chunk -> (
          match VM.run_vm chunk with
          | Ok (Value.VBytecodeClosure { chunk = fn_chunk; _ }) ->
              Alcotest.(check bool)
                "compiled closure has instructions" true
                (Bytecode.length fn_chunk > 0)
          | _ -> Alcotest.fail "Expected Value.VBytecodeClosure from run_vm"))

let test_compile_fun_error_propagation () =
  let valid_span =
    { Ast.file = "<test>"; start_line = 1; start_col = 0; end_line = 1; end_col = 10 }
  in
  (* Malformed Core_ir expr with bad primop arity inside a lambda body *)
  let bad_primop =
    Core_ir.make_expr ~span:valid_span
      (PrimOp { op = Add; args = [ Core_ir.make_expr ~span:valid_span (Lit (Int 1)) ] })
  in
  (* Compiler.compile_fun must fail and not swallow the error *)
  (match Compiler.compile_fun ~param:"x" bad_primop with
  | Error err ->
      Alcotest.(check string)
        "compile_fun propagates error" "Invalid primop arity for +" err.message
  | Ok _ -> Alcotest.fail "Expected compile_fun to fail on invalid primop arity");
  (* Whole expr compilation with Fun must also propagate the error *)
  let fun_expr =
    Core_ir.make_expr ~span:valid_span (Fun { param = "x"; body = bad_primop })
  in
  match Compiler.compile_expr fun_expr with
  | Error err ->
      Alcotest.(check string)
        "compile_expr propagates error" "PrimOp '+' expects 2 arguments but received 1"
        err.message
  | Ok _ -> Alcotest.fail "Expected compile_expr to fail on invalid lambda body"

let () =
  let open Alcotest in
  run "Vesper Bytecode VM Unit Tests"
    [
      ( "ISA & Opcodes",
        [ test_case "Opcode operations and equality" `Quick test_opcode_operations ] );
      ( "Bytecode Chunks",
        [ test_case "Chunk structure and inspection" `Quick test_chunk_operations ] );
      ( "Compiler & VM Execution",
        [ test_case "Basic compilation and execution" `Quick test_compiler_and_vm_basic ]
      );
      ( "VM Error Handling",
        [ test_case "Runtime safety and error traps" `Quick test_vm_errors ] );
      ( "Static Closures",
        [
          test_case "Static closures and pre-compiled functions" `Quick
            test_static_closures;
        ] );
      ( "Compiler Error Propagation",
        [
          test_case "compile_fun error propagation" `Quick
            test_compile_fun_error_propagation;
        ] );
    ]
