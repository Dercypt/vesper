open Vesper

(** Invariant test for Law 5: Deterministic Evaluation & Sound Termination. Execution of a
    closed expression under a fixed environment must be fully deterministic. Evaluator
    transitions, final values, step counts, and error states must be bit-for-bit identical
    across repeated executions without relying on ambient state or host entropy. *)

let parse_and_desugar src =
  match Parse_facade.parse_string ~file:"determinism.vesper" src with
  | Error diag -> Error diag.message
  | Ok ast -> (
      match Desugar.lower ast with Error err -> Error err.message | Ok core -> Ok core)

let test_handcrafted_determinism () =
  let programs =
    [
      ( "recursive fibonacci",
        "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2); fib 10;" );
      ( "recursive factorial",
        "let rec fact n = if n <= 1 then 1 else n * fact (n - 1); fact 7;" );
      ( "higher-order closure composition",
        "let compose f g x = f (g x); let add1 x = x + 1; let mul2 x = x * 2; compose \
         mul2 add1 15;" );
      ( "lexical scope shadowing",
        "let x = 10; let y = (let x = 20 in let z = x * 2 in z + x); x + y;" );
      ( "curried multi-argument application",
        "let rec ack m n = if m == 0 then n + 1 else if n == 0 then ack (m - 1) 1 else \
         ack (m - 1) (ack m (n - 1)); ack 2 3;" );
      ( "short-circuit logic with potential divzero",
        "let a = false && (1 / 0 == 0); let b = true || (1 / 0 == 0); if b then 42 else \
         0;" );
      ( "safe runtime division by zero error determinism",
        "let rec f n = if n == 0 then 10 / 0 else f (n - 1); f 5;" );
      ( "fuel exhaustion error determinism",
        "let rec infinite x = infinite (x + 1); infinite 0;" );
    ]
  in
  List.iter
    (fun (name, src) ->
      match parse_and_desugar src with
      | Error err -> Alcotest.fail (Printf.sprintf "%s: setup failed: %s" name err)
      | Ok core_prog ->
          let fuel = 50_000 in
          let baseline = Eval.eval_program_with_stats ~fuel core_prog in
          for iter = 2 to 10 do
            let current = Eval.eval_program_with_stats ~fuel core_prog in
            match (baseline, current) with
            | Ok b_out, Ok c_out ->
                if not (Value.equal b_out.value c_out.value) then
                  Alcotest.fail
                    (Printf.sprintf "%s: value divergence on iteration %d: %s vs %s" name
                       iter (Value.to_string b_out.value) (Value.to_string c_out.value));
                if b_out.steps <> c_out.steps then
                  Alcotest.fail
                    (Printf.sprintf "%s: step count divergence on iteration %d: %d vs %d"
                       name iter b_out.steps c_out.steps);
                Alcotest.(check string)
                  (Printf.sprintf "%s: string repr identical (iter %d)" name iter)
                  (Value.to_string b_out.value) (Value.to_string c_out.value)
            | Error b_err, Error c_err ->
                Alcotest.(check string)
                  (Printf.sprintf "%s: error message identical (iter %d)" name iter)
                  b_err.message c_err.message;
                Alcotest.(check string)
                  (Printf.sprintf "%s: error span identical (iter %d)" name iter)
                  (Ast.span_to_string b_err.span)
                  (Ast.span_to_string c_err.span)
            | Ok _, Error err ->
                Alcotest.fail
                  (Printf.sprintf "%s: expected Ok on iter %d, but got Error %s" name iter
                     err.message)
            | Error err, Ok _ ->
                Alcotest.fail
                  (Printf.sprintf "%s: expected Error %s on iter %d, but got Ok" name
                     err.message iter)
          done)
    programs

let test_fuzz_100_determinism () =
  Random.init 12345;
  for prog_idx = 1 to 100 do
    let ast_prog = Test_law2_roundtrip.gen_program 3 in
    match Desugar.lower ast_prog with
    | Error _ -> ()
    | Ok core_prog ->
        let fuel = 500 in
        let baseline = Eval.eval_program_with_stats ~fuel core_prog in
        for iter = 2 to 10 do
          let current = Eval.eval_program_with_stats ~fuel core_prog in
          match (baseline, current) with
          | Ok b_out, Ok c_out ->
              if not (Value.equal b_out.value c_out.value) then
                Alcotest.fail
                  (Printf.sprintf "Fuzz program %d: value divergence on iter %d: %s vs %s"
                     prog_idx iter (Value.to_string b_out.value)
                     (Value.to_string c_out.value));
              if b_out.steps <> c_out.steps then
                Alcotest.fail
                  (Printf.sprintf "Fuzz program %d: step divergence on iter %d: %d vs %d"
                     prog_idx iter b_out.steps c_out.steps)
          | Error b_err, Error c_err ->
              if not (String.equal b_err.message c_err.message) then
                Alcotest.fail
                  (Printf.sprintf
                     "Fuzz program %d: error message divergence on iter %d: %S vs %S"
                     prog_idx iter b_err.message c_err.message);
              if not (Ast.span_to_string b_err.span = Ast.span_to_string c_err.span) then
                Alcotest.fail
                  (Printf.sprintf
                     "Fuzz program %d: error span divergence on iter %d: %s vs %s"
                     prog_idx iter
                     (Ast.span_to_string b_err.span)
                     (Ast.span_to_string c_err.span))
          | Ok _, Error err ->
              Alcotest.fail
                (Printf.sprintf "Fuzz program %d: Ok turned to Error %s on iter %d"
                   prog_idx err.message iter)
          | Error err, Ok _ ->
              Alcotest.fail
                (Printf.sprintf "Fuzz program %d: Error %s turned to Ok on iter %d"
                   prog_idx err.message iter)
        done
  done

let test_step_count_monotonicity () =
  let make_chain n =
    let rec build i = if i <= 1 then "1" else Printf.sprintf "(%s + 1)" (build (i - 1)) in
    Printf.sprintf "let x = %s; x;" (build n)
  in
  let run n =
    match parse_and_desugar (make_chain n) with
    | Error err -> Alcotest.fail err
    | Ok core -> (
        match Eval.eval_program_with_stats core with
        | Ok out -> out.steps
        | Error err -> Alcotest.fail err.message)
  in
  let s5 = run 5 in
  let s10 = run 10 in
  let s20 = run 20 in
  Alcotest.(check bool) "Steps increase with depth: s5 < s10" true (s5 < s10);
  Alcotest.(check bool) "Steps increase with depth: s10 < s20" true (s10 < s20)

let tests =
  [
    Alcotest.test_case "Handcrafted complex programs across 10 iterations (Law 5)" `Quick
      test_handcrafted_determinism;
    Alcotest.test_case
      "100 fuzz programs across 10 iterations bit-for-bit determinism (Law 5)" `Quick
      test_fuzz_100_determinism;
    Alcotest.test_case "Step count reproducibility and monotonicity" `Quick
      test_step_count_monotonicity;
  ]
