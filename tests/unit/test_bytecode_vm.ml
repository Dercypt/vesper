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

let test_bytecode_closure_operations () =
  let chunk =
    Bytecode.create
      ~code:[| Opcode.Op_GetLocal 0; Opcode.Op_Return |]
      ~constants:[||] ~spans:[| dummy_span; dummy_span |] ~param:"x" ()
  in
  let clos = Value.bytecode_closure ~chunk ~env:[] in
  Alcotest.(check string) "type_of closure" "function" (Value.type_of clos);
  Alcotest.(check string) "to_string closure" "<fun x>" (Value.to_string clos);
  let clos2 = Value.bytecode_closure ~chunk ~env:[] in
  Alcotest.(check bool) "equal closure self" true (Value.equal clos clos2)

let test_core_ir_free_vars () =
  let span = dummy_span in
  let mk_var name = Core_ir.make_expr ~span (Var name) in
  let mk_int n = Core_ir.make_expr ~span (Lit (Int n)) in
  let mk_add a b = Core_ir.make_expr ~span (PrimOp { op = Add; args = [ a; b ] }) in
  (* Literals *)
  Alcotest.(check (list string)) "lit has no free vars" [] (Core_ir.free_vars (mk_int 42));
  (* Var *)
  Alcotest.(check (list string))
    "var has free var" [ "x" ]
    (Core_ir.free_vars (mk_var "x"));
  (* PrimOp *)
  Alcotest.(check (list string))
    "primop combines free vars" [ "x"; "y" ]
    (Core_ir.free_vars (mk_add (mk_var "y") (mk_var "x")));
  (* Fun binds parameter *)
  let fun1 =
    Core_ir.make_expr ~span (Fun { param = "x"; body = mk_add (mk_var "x") (mk_int 1) })
  in
  Alcotest.(check (list string)) "fun binds param" [] (Core_ir.free_vars fun1);
  (* Curried inner fun retains outer param *)
  let inner_body = mk_add (mk_var "x") (mk_var "y") in
  let inner_fun = Core_ir.make_expr ~span (Fun { param = "y"; body = inner_body }) in
  Alcotest.(check (list string))
    "inner fun free vars" [ "x" ] (Core_ir.free_vars inner_fun);
  let outer_fun = Core_ir.make_expr ~span (Fun { param = "x"; body = inner_fun }) in
  Alcotest.(check (list string)) "outer fun binds both" [] (Core_ir.free_vars outer_fun);
  (* Let binding *)
  let let_nonrec =
    Core_ir.make_expr ~span
      (Let
         {
           name = "a";
           is_rec = false;
           value = mk_var "z";
           body = mk_add (mk_var "a") (mk_var "b");
         })
  in
  Alcotest.(check (list string))
    "let nonrec free vars" [ "b"; "z" ]
    (Core_ir.free_vars let_nonrec);
  (* Let rec binding *)
  let let_rec =
    Core_ir.make_expr ~span
      (Let
         {
           name = "f";
           is_rec = true;
           value = mk_add (mk_var "f") (mk_var "k");
           body = mk_var "f";
         })
  in
  Alcotest.(check (list string)) "let rec free vars" [ "k" ] (Core_ir.free_vars let_rec)

let test_compiler_free_var_captures () =
  let src = "let unused = 0; let f x = x + 1; f;" in
  match Parse_facade.parse_string ~file:"<test>" src with
  | Error diag -> Alcotest.fail diag.message
  | Ok ast -> (
      (match Compiler.compile ast with
      | Error err -> Alcotest.fail err.message
      | Ok main_chunk ->
          Alcotest.(check int) "main has child chunk" 1 (Array.length main_chunk.children);
          let fn_chunk = main_chunk.children.(0) in
          Alcotest.(check int)
            "f chunk captures nothing" 0
            (Array.length fn_chunk.captures));
      let src2 = "let f a b = let add_a x = a + x in add_a;" in
      match Parse_facade.parse_string ~file:"<test>" src2 with
      | Error diag -> Alcotest.fail diag.message
      | Ok ast2 -> (
          match Compiler.compile ast2 with
          | Error err -> Alcotest.fail err.message
          | Ok main_chunk2 ->
              Alcotest.(check int)
                "main has child chunk" 1
                (Array.length main_chunk2.children);
              let f_chunk = main_chunk2.children.(0) in
              Alcotest.(check int) "f has child chunk" 1 (Array.length f_chunk.children);
              let add_a_chunk = f_chunk.children.(0) in
              Alcotest.(check int)
                "add_a captures only a" 1
                (Array.length add_a_chunk.captures);
              let cap_name, _ = add_a_chunk.captures.(0) in
              Alcotest.(check string) "captured var name is a" "a" cap_name))

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
          test_case "Bytecode closure operations" `Quick test_bytecode_closure_operations;
          test_case "Core IR free variables computation" `Quick test_core_ir_free_vars;
          test_case "Compiler filters captures to free variables" `Quick
            test_compiler_free_var_captures;
        ] );
      ( "Compiler Error Propagation",
        [
          test_case "compile_fun error propagation" `Quick
            test_compile_fun_error_propagation;
        ] );
    ]
