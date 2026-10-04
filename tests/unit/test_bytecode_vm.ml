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
  match run_src "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 5;" with
  | Ok (Value.VInt 120) -> ()
  | _ -> Alcotest.fail "Expected 120"

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
  match run_src "let x = 1; x 2;" with
  | Error msg ->
      Alcotest.(check bool) "type error" true (String.sub msg 0 10 = "Type error")
  | Ok () -> Alcotest.fail "Expected type error"

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
    ]
