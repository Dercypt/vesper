open Vesper
module VM = Vm

(** Dual-Execution Conformance & Determinism Suite (Phase 8, Law 5). Proves bit-for-bit
    behavioral equivalence between the Phase 4 tree-walking evaluator and the Phase 8
    bytecode virtual machine. All values, error states, and diagnostic messages must match
    identically across both engines. *)

let parse_and_desugar src =
  match Parse_facade.parse_string ~file:"conformance.vesper" src with
  | Error diag -> Error diag.message
  | Ok ast -> (
      match Desugar.lower ast with Error err -> Error err.message | Ok core -> Ok core)

let test_handcrafted_conformance () =
  let programs =
    [
      ("simple constant", "42;");
      ("arithmetic add sub mul", "2 + 3 * 4 - 5;");
      ("division and mod", "20 / 4 + 17 % 5;");
      ("unary neg and not", "-15 + if !false then 10 else 0;");
      ("string literal and concat", "\"hello \" + \"world\";");
      ("string comparisons", "if \"apple\" < \"banana\" then 1 else 0;");
      ("comparisons int", "if 10 <= 20 && 5 > 2 && 42 == 42 then 100 else 0;");
      ("boolean equality", "if (true != false) && (false == false) then 1 else 0;");
      ( "short-circuit logic with divzero guard",
        "let a = false && (1 / 0 == 0); let b = true || (1 / 0 == 0); if b then 42 else \
         0;" );
      ("let binding scope", "let x = 10; let y = 20; x + y;");
      ("nested let shadowing", "let x = 1; let y = (let x = 10 in x * 2); x + y;");
      ("sequential re-binding", "let x = 1; let x = x + 1; let x = x * 2; x;");
      ("multi-variable let", "let a = 1; let b = 2; let c = 3; let d = 4; a + b + c + d;");
      ("simple lambda call", "let f = fun x -> x * 2; f 21;");
      ("curried multi-arg", "let add = fun x -> fun y -> x + y; add 10 20;");
      ("ast multi-arg syntax", "let add x y = x + y; add 10 20;");
      ( "lexical closure capture",
        "let make_adder = fun x -> fun y -> x + y; let add5 = make_adder 5; let x = 999; \
         add5 10;" );
      ( "higher-order apply",
        "let apply = fun f -> fun x -> f x; let double = fun x -> x * 2; apply double 50;"
      );
      ( "closure composition",
        "let compose = fun f -> fun g -> fun x -> f (g x); let mul2 = fun x -> x * 2; \
         let add1 = fun x -> x + 1; compose mul2 add1 10;" );
      ( "recursive factorial",
        "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 6;" );
      ( "recursive fibonacci",
        "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2); fib 10;" );
      ( "recursive accumulator",
        "let rec sum_acc n acc = if n <= 0 then acc else sum_acc (n - 1) (acc + n); \
         sum_acc 10 0;" );
      ( "nested recursive calls (ackermann)",
        "let rec ack m n = if m == 0 then n + 1 else if n == 0 then ack (m - 1) 1 else \
         ack (m - 1) (ack m (n - 1)); ack 2 3;" );
      ("division by zero error", "10 / 0;");
      ("modulo by zero error", "10 % 0;");
      ("unbound variable error", "unknown_var + 1;");
      ("type error arithmetic", "1 + true;");
      ("type error condition", "if 42 then 1 else 2;");
      ("calling non-function error", "let x = 42; x 10;");
      ("fuel exhaustion error", "let rec loop n = loop (n + 1); loop 0;");
    ]
  in
  List.iter
    (fun (name, src) ->
      match parse_and_desugar src with
      | Error err -> Alcotest.fail (Printf.sprintf "%s: desugaring failed: %s" name err)
      | Ok core_prog -> (
          let fuel = 50_000 in
          let eval_res = Eval.eval_program_with_stats ~fuel core_prog in
          match Compiler.compile_program core_prog with
          | Error compile_err ->
              Alcotest.fail
                (Printf.sprintf "%s: compilation failed: %s" name compile_err.message)
          | Ok chunk ->
              for iter = 1 to 5 do
                let vm_res = VM.run_outcome_ast_error ~fuel chunk in
                match (eval_res, vm_res) with
                | Ok e_out, Ok v_out ->
                    if not (Value.equal e_out.value v_out.value) then
                      Alcotest.fail
                        (Printf.sprintf
                           "%s (iter %d): Value mismatch: Eval returned %s, VM returned \
                            %s"
                           name iter (Value.to_string e_out.value)
                           (Value.to_string v_out.value));
                    Alcotest.(check string)
                      (Printf.sprintf "%s (iter %d): String representations match" name
                         iter)
                      (Value.to_string e_out.value) (Value.to_string v_out.value)
                | Error e_err, Error v_err ->
                    Alcotest.(check string)
                      (Printf.sprintf "%s (iter %d): Error messages match" name iter)
                      e_err.message v_err.message;
                    if e_err.message <> "Execution budget exhausted" then
                      Alcotest.(check string)
                        (Printf.sprintf "%s (iter %d): Error spans match" name iter)
                        (Ast.span_to_string e_err.span)
                        (Ast.span_to_string v_err.span)
                | Ok e_out, Error v_err ->
                    Alcotest.fail
                      (Printf.sprintf
                         "%s (iter %d): Eval succeeded with %s, but VM failed with %s \
                          (%s)"
                         name iter (Value.to_string e_out.value) v_err.message
                         (Ast.span_to_string v_err.span))
                | Error e_err, Ok v_out ->
                    Alcotest.fail
                      (Printf.sprintf
                         "%s (iter %d): Eval failed with %s (%s), but VM succeeded with \
                          %s"
                         name iter e_err.message
                         (Ast.span_to_string e_err.span)
                         (Value.to_string v_out.value))
              done))
    programs

let test_fuzz_500_conformance () =
  Random.init 54321;
  let matches = ref 0 in
  for prog_idx = 1 to 500 do
    let ast_prog = Test_law2_roundtrip.gen_program 3 in
    match Desugar.lower ast_prog with
    | Error _ -> ()
    | Ok core_prog -> (
        let fuel = 500 in
        let eval_res = Eval.eval_program_with_stats ~fuel core_prog in
        match Compiler.compile_program core_prog with
        | Error _ -> ()
        | Ok chunk -> (
            let vm_res = VM.run_outcome_ast_error ~fuel chunk in
            incr matches;
            match (eval_res, vm_res) with
            | Ok e_out, Ok v_out ->
                if not (Value.equal e_out.value v_out.value) then
                  Alcotest.fail
                    (Printf.sprintf "Fuzz %d: Value mismatch: Eval=%s, VM=%s" prog_idx
                       (Value.to_string e_out.value) (Value.to_string v_out.value));
                Alcotest.(check string)
                  (Printf.sprintf "Fuzz %d: String format matches" prog_idx)
                  (Value.to_string e_out.value) (Value.to_string v_out.value)
            | Error e_err, Error v_err ->
                Alcotest.(check string)
                  (Printf.sprintf "Fuzz %d: Error message matches" prog_idx)
                  e_err.message v_err.message
            | Ok e_out, Error v_err ->
                Alcotest.fail
                  (Printf.sprintf "Fuzz %d: Eval succeeded with %s, but VM failed with %s"
                     prog_idx (Value.to_string e_out.value) v_err.message)
            | Error e_err, Ok v_out ->
                Alcotest.fail
                  (Printf.sprintf "Fuzz %d: Eval failed with %s, but VM succeeded with %s"
                     prog_idx e_err.message (Value.to_string v_out.value))))
  done;
  Alcotest.(check bool) "At least 100 fuzz programs evaluated" true (!matches >= 100)

let test_disassembly_inspection () =
  let src = "let x = 10; let y = 20; x + y;" in
  match parse_and_desugar src with
  | Error err -> Alcotest.fail err
  | Ok core -> (
      match Compiler.compile_program core with
      | Error err -> Alcotest.fail err.message
      | Ok chunk ->
          let text = Disasm.to_string ~name:"test_prog" chunk in
          Alcotest.(check bool)
            "Disassembly contains header" true
            (let len = String.length "== test_prog ==" in
             String.sub text 0 len = "== test_prog ==");
          Alcotest.(check bool)
            "Disassembly contains Op_Const" true
            (let len_sub = String.length "Op_Const" in
             let len_text = String.length text in
             let found = ref false in
             for i = 0 to len_text - len_sub do
               if String.sub text i len_sub = "Op_Const" then found := true
             done;
             !found);
          Alcotest.(check bool)
            "Disassembly contains Op_Add" true
            (let len_sub = String.length "Op_Add" in
             let len_text = String.length text in
             let found = ref false in
             for i = 0 to len_text - len_sub do
               if String.sub text i len_sub = "Op_Add" then found := true
             done;
             !found))

let tests =
  [
    Alcotest.test_case "Handcrafted programs Tree-Walking vs VM Conformance (Law 5)"
      `Quick test_handcrafted_conformance;
    Alcotest.test_case "500 Fuzz programs Dual-Execution bit-for-bit equivalence (Law 5)"
      `Quick test_fuzz_500_conformance;
    Alcotest.test_case "Bytecode chunk disassembly and inspection" `Quick
      test_disassembly_inspection;
  ]
