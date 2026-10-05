open Vesper

(** Law 1 (Total Diagnosability & Host Isolation) & Law 5 (Deterministic Evaluation)
    Verification Suite for Sandboxed I/O and Standard Library. *)

let test_deterministic_io_replay () =
  let initial_vfs =
    [
      ("/sandbox/data.txt", "immutable payload for testing\nline 2\n");
      ("/sandbox/config.json", "{\"key\": \"val\", \"count\": 42}");
    ]
  in
  let run_simulation () =
    let env =
      Effect_runtime.create_env ~stdin_str:"user_input_token_99\nsecond line input\n"
        ~fs_root:"/sandbox" ~vfs:initial_vfs ()
    in
    (* Step 1: Read from stdin *)
    let l1 = match Effect_runtime.read_line env with Ok s -> s | Error _ -> "" in
    Effect_runtime.print_string env ("Read line 1: " ^ l1 ^ "\n");

    (* Step 2: Read from VFS *)
    let data =
      match Effect_runtime.read_file env "/sandbox/data.txt" with
      | Ok s -> s
      | Error _ -> ""
    in
    Effect_runtime.print_string env ("Read data: " ^ data);

    (* Step 3: Compute with Core_math and emit to stdout *)
    let hash_val =
      List.fold_left
        (fun acc c -> Core_math.add (Core_math.mul acc 31) (Char.code c))
        0
        (Core_string.to_char_list (l1 ^ data))
    in
    Effect_runtime.print_string env "Computed hash: ";
    Effect_runtime.print_int env hash_val;
    Effect_runtime.print_endline env "";

    (* Step 4: Write new file and read back *)
    let write_res =
      Effect_runtime.write_file env "/sandbox/out.txt" ("Hash: " ^ string_of_int hash_val)
    in
    assert (Result.is_ok write_res);

    let reread =
      match Effect_runtime.read_file env "/sandbox/out.txt" with
      | Ok s -> s
      | Error _ -> ""
    in
    Effect_runtime.print_endline env ("Reread: " ^ reread);

    (Effect_runtime.get_stdout env, Effect_runtime.get_stderr env)
  in
  (* Run the simulation 50 times and assert bit-for-bit identity *)
  let expected_out, expected_err = run_simulation () in
  for iter = 1 to 50 do
    let actual_out, actual_err = run_simulation () in
    Alcotest.(check string)
      (Printf.sprintf "Iter %d: Stdout bit-for-bit replay identity" iter)
      expected_out actual_out;
    Alcotest.(check string)
      (Printf.sprintf "Iter %d: Stderr bit-for-bit replay identity" iter)
      expected_err actual_err
  done

let test_adversarial_traversal_rejections () =
  let env =
    Effect_runtime.create_env ~fs_root:"/isolated/sandbox"
      ~vfs:[ ("/isolated/sandbox/safe.txt", "safe") ]
      ()
  in
  let malicious_paths =
    [
      "../etc/passwd";
      "../../secret.key";
      "../../../etc/shadow";
      "/etc/passwd";
      "/var/log/../../etc/hosts";
      "sub/../../../../root/.bashrc";
      "foo/bar/../../../outside";
      "/isolated/sandbox/../../outside";
      "/isolated/sandbox/sub/../../../../etc";
    ]
  in
  List.iter
    (fun path ->
      (* Adversarial read *)
      (match Effect_runtime.read_file env path with
      | Error diag ->
          Alcotest.(check bool)
            (Printf.sprintf "Read '%s' rejected with E0001" path)
            true
            (diag.code = Error_code.E0001_io_error && String.length diag.message > 0)
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Security failure: malicious read '%s' succeeded" path));

      (* Adversarial write *)
      (match Effect_runtime.write_file env path "hacked" with
      | Error diag ->
          Alcotest.(check bool)
            (Printf.sprintf "Write '%s' rejected with E0001" path)
            true
            (diag.code = Error_code.E0001_io_error && String.length diag.message > 0)
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Security failure: malicious write '%s' succeeded" path));

      (* Adversarial delete *)
      match Effect_runtime.delete_file env path with
      | Error diag ->
          Alcotest.(check bool)
            (Printf.sprintf "Delete '%s' rejected with E0001" path)
            true
            (diag.code = Error_code.E0001_io_error && String.length diag.message > 0)
      | Ok _ ->
          Alcotest.fail
            (Printf.sprintf "Security failure: malicious delete '%s' succeeded" path))
    malicious_paths

let test_unconfigured_root_rejections () =
  (* No fs_root capability configured *)
  let env = Effect_runtime.create_env () in
  match Effect_runtime.read_file env "file.txt" with
  | Error diag ->
      Alcotest.(check bool)
        "Access denied when no fs_root" true
        (diag.code = Error_code.E0001_io_error)
  | Ok _ -> Alcotest.fail "Expected access denied error"

let test_prelude_determinism_and_conformance () =
  let prog_src =
    "let a = abs (-42);\n\
     let b = min 100 200;\n\
     let c = max 100 200;\n\
     let d = clamp 0 50 75;\n\
     let e = sign (-10);\n\
     let f = succ (pred 10);\n\
     a + b + c + d + e + f;\n"
  in
  match Parse_facade.parse_string ~file:"<test>" prog_src with
  | Error diag -> Alcotest.fail diag.message
  | Ok ast -> (
      let ast_with_prelude = Prelude.with_prelude ast in
      match Desugar.lower ast_with_prelude with
      | Error err -> Alcotest.fail err.message
      | Ok core_prog -> (
          let eval_res = Eval.eval_program core_prog in
          match Compiler.compile ast_with_prelude with
          | Error err -> Alcotest.fail err.message
          | Ok chunk ->
              for iter = 1 to 20 do
                let vm_res = Vm.run_outcome_ast_error chunk in
                match (eval_res, vm_res) with
                | Ok eval_val, Ok vm_outcome ->
                    Alcotest.(check bool)
                      (Printf.sprintf "Iter %d: Value equal" iter)
                      true
                      (Value.equal eval_val vm_outcome.value);
                    Alcotest.(check string)
                      (Printf.sprintf "Iter %d: String equal" iter)
                      (Value.to_string eval_val)
                      (Value.to_string vm_outcome.value)
                | _ -> Alcotest.fail "Expected both eval and VM to succeed"
              done))

let tests =
  [
    Alcotest.test_case "Deterministic I/O Replay Suite (Law 5)" `Quick
      test_deterministic_io_replay;
    Alcotest.test_case "Adversarial Directory Traversal Attack Rejection (Law 1)" `Quick
      test_adversarial_traversal_rejections;
    Alcotest.test_case "Unconfigured Sandbox Capability Rejection (Law 1)" `Quick
      test_unconfigured_root_rejections;
    Alcotest.test_case "Prelude Conformance & Determinism (Law 5)" `Quick
      test_prelude_determinism_and_conformance;
  ]
