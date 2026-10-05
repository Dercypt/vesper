open Vesper

let test_core_math () =
  Alcotest.(check int) "abs positive" 42 (Core_math.abs 42);
  Alcotest.(check int) "abs negative" 42 (Core_math.abs (-42));
  Alcotest.(check int) "abs zero" 0 (Core_math.abs 0);
  Alcotest.(check int) "abs min_int safe" Int.max_int (Core_math.abs Int.min_int);
  Alcotest.(check int) "add" 15 (Core_math.add 7 8);
  Alcotest.(check int) "sub" (-1) (Core_math.sub 7 8);
  Alcotest.(check int) "mul" 56 (Core_math.mul 7 8);
  (match Core_math.div 20 4 with
  | Ok v -> Alcotest.(check int) "div 20 4" 5 v
  | Error _ -> Alcotest.fail "Expected division success");
  (match Core_math.div 20 0 with
  | Error msg -> Alcotest.(check string) "div zero" "Division by zero" msg
  | Ok _ -> Alcotest.fail "Expected division by zero error");
  (match Core_math.rem 23 5 with
  | Ok v -> Alcotest.(check int) "rem 23 5" 3 v
  | Error _ -> Alcotest.fail "Expected rem success");
  (match Core_math.rem 23 0 with
  | Error msg -> Alcotest.(check string) "rem zero" "Division by zero" msg
  | Ok _ -> Alcotest.fail "Expected rem by zero error");
  (match Core_math.pow 2 10 with
  | Ok v -> Alcotest.(check int) "pow 2 10" 1024 v
  | Error _ -> Alcotest.fail "Expected pow success");
  (match Core_math.pow 2 (-1) with
  | Error msg -> Alcotest.(check string) "pow neg" "Negative exponent" msg
  | Ok _ -> Alcotest.fail "Expected neg exponent error");
  Alcotest.(check int) "min" 5 (Core_math.min 5 10);
  Alcotest.(check int) "max" 10 (Core_math.max 5 10);
  Alcotest.(check int) "clamp mid" 5 (Core_math.clamp ~min:0 ~max:10 5);
  Alcotest.(check int) "clamp low" 0 (Core_math.clamp ~min:0 ~max:10 (-5));
  Alcotest.(check int) "clamp high" 10 (Core_math.clamp ~min:0 ~max:10 15);
  Alcotest.(check int) "sign neg" (-1) (Core_math.sign (-99));
  Alcotest.(check int) "sign zero" 0 (Core_math.sign 0);
  Alcotest.(check int) "sign pos" 1 (Core_math.sign 99);
  Alcotest.(check int) "succ" 43 (Core_math.succ 42);
  Alcotest.(check int) "pred" 41 (Core_math.pred 42);
  Alcotest.(check bool) "is_zero" true (Core_math.is_zero 0);
  Alcotest.(check bool) "is_positive" true (Core_math.is_positive 1);
  Alcotest.(check bool) "is_negative" true (Core_math.is_negative (-1));
  Alcotest.(check bool) "is_even" true (Core_math.is_even 4);
  Alcotest.(check bool) "is_odd" true (Core_math.is_odd 5)

let test_core_string () =
  Alcotest.(check int) "length" 5 (Core_string.length "hello");
  Alcotest.(check bool) "is_empty false" false (Core_string.is_empty "hello");
  Alcotest.(check bool) "is_empty true" true (Core_string.is_empty "");
  Alcotest.(check string)
    "concat" "a, b, c"
    (Core_string.concat ~sep:", " [ "a"; "b"; "c" ]);
  Alcotest.(check (list string))
    "split" [ "foo"; "bar"; "baz" ]
    (Core_string.split ~on:':' "foo:bar:baz");
  Alcotest.(check (list string))
    "split_lines" [ "line1"; "line2"; "line3" ]
    (Core_string.split_lines "line1\nline2\r\nline3");
  Alcotest.(check string) "trim" "hello world" (Core_string.trim "   hello world \t\n");
  (match Core_string.slice "abcdef" ~start:1 ~len:3 with
  | Ok s -> Alcotest.(check string) "slice ok" "bcd" s
  | Error _ -> Alcotest.fail "Expected slice success");
  (match Core_string.slice "abcdef" ~start:4 ~len:10 with
  | Error msg -> Alcotest.(check string) "slice out of bounds" "Index out of bounds" msg
  | Ok _ -> Alcotest.fail "Expected out of bounds error");
  Alcotest.(check bool)
    "starts_with" true
    (Core_string.starts_with ~prefix:"vesp" "vesper");
  Alcotest.(check bool)
    "starts_with false" false
    (Core_string.starts_with ~prefix:"xyz" "vesper");
  Alcotest.(check bool) "ends_with" true (Core_string.ends_with ~suffix:"per" "vesper");
  Alcotest.(check bool) "contains" true (Core_string.contains ~sub:"spe" "vesper");
  Alcotest.(check (option int))
    "index_of" (Some 2)
    (Core_string.index_of "vesper" ~sub:"sp");
  Alcotest.(check (option int))
    "index_of none" None
    (Core_string.index_of "vesper" ~sub:"abc");
  Alcotest.(check string) "of_int" "123" (Core_string.of_int 123);
  Alcotest.(check (option int)) "to_int" (Some 123) (Core_string.to_int "123");
  Alcotest.(check (option int)) "to_int invalid" None (Core_string.to_int "abc");
  Alcotest.(check string) "of_bool" "true" (Core_string.of_bool true);
  Alcotest.(check (option bool)) "to_bool" (Some false) (Core_string.to_bool "false");
  let chars = Core_string.to_char_list "abc" in
  Alcotest.(check (list char)) "to_char_list" [ 'a'; 'b'; 'c' ] chars;
  Alcotest.(check string) "of_char_list" "abc" (Core_string.of_char_list chars)

let test_core_list () =
  let l = [ 1; 2; 3; 4; 5 ] in
  Alcotest.(check int) "length" 5 (Core_list.length l);
  Alcotest.(check bool) "is_empty false" false (Core_list.is_empty l);
  Alcotest.(check bool) "is_empty true" true (Core_list.is_empty []);
  Alcotest.(check (option int)) "head" (Some 1) (Core_list.head l);
  Alcotest.(check (option (list int))) "tail" (Some [ 2; 3; 4; 5 ]) (Core_list.tail l);
  Alcotest.(check (list int)) "cons" [ 0; 1; 2; 3; 4; 5 ] (Core_list.cons 0 l);
  Alcotest.(check (list int)) "map" [ 2; 4; 6; 8; 10 ] (Core_list.map (fun x -> x * 2) l);
  Alcotest.(check (list int))
    "filter" [ 2; 4 ]
    (Core_list.filter (fun x -> x mod 2 = 0) l);
  Alcotest.(check int) "fold_left" 15 (Core_list.fold_left ( + ) 0 l);
  Alcotest.(check int) "fold_right" 15 (Core_list.fold_right ( + ) l 0);
  Alcotest.(check (list int)) "reverse" [ 5; 4; 3; 2; 1 ] (Core_list.reverse l);
  Alcotest.(check (list int))
    "append" [ 1; 2; 3; 4; 5; 6; 7 ]
    (Core_list.append l [ 6; 7 ]);
  Alcotest.(check (list (pair int string)))
    "zip"
    [ (1, "a"); (2, "b"); (3, "c") ]
    (Core_list.zip [ 1; 2; 3 ] [ "a"; "b"; "c"; "d" ]);
  Alcotest.(check (list int))
    "zip_with" [ 5; 7; 9 ]
    (Core_list.zip_with ( + ) [ 1; 2; 3 ] [ 4; 5; 6 ]);
  Alcotest.(check (list int)) "take" [ 1; 2; 3 ] (Core_list.take 3 l);
  Alcotest.(check (list int)) "drop" [ 4; 5 ] (Core_list.drop 3 l);
  Alcotest.(check (option int)) "nth" (Some 3) (Core_list.nth 2 l);
  Alcotest.(check (option int)) "nth out of bounds" None (Core_list.nth 10 l);
  Alcotest.(check (option int)) "find" (Some 4) (Core_list.find (fun x -> x > 3) l);
  Alcotest.(check bool) "exists" true (Core_list.exists (fun x -> x = 3) l);
  Alcotest.(check bool) "for_all" true (Core_list.for_all (fun x -> x > 0) l);
  let evens, odds = Core_list.partition (fun x -> x mod 2 = 0) l in
  Alcotest.(check (list int)) "partition evens" [ 2; 4 ] evens;
  Alcotest.(check (list int)) "partition odds" [ 1; 3; 5 ] odds

let test_core_map () =
  let m = Core_map.empty in
  Alcotest.(check bool) "is_empty true" true (Core_map.is_empty m);
  let m = Core_map.add "alpha" 10 m in
  let m = Core_map.add "beta" 20 m in
  let m = Core_map.add "gamma" 30 m in
  Alcotest.(check bool) "is_empty false" false (Core_map.is_empty m);
  Alcotest.(check int) "cardinal" 3 (Core_map.cardinal m);
  Alcotest.(check (option int)) "find_opt" (Some 20) (Core_map.find_opt "beta" m);
  (match Core_map.find "alpha" m with
  | Ok v -> Alcotest.(check int) "find ok" 10 v
  | Error _ -> Alcotest.fail "Expected find success");
  (match Core_map.find "delta" m with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected find failure");
  Alcotest.(check bool) "mem true" true (Core_map.mem "gamma" m);
  Alcotest.(check bool) "mem false" false (Core_map.mem "zeta" m);
  let m2 = Core_map.remove "beta" m in
  Alcotest.(check int) "cardinal after remove" 2 (Core_map.cardinal m2);
  Alcotest.(check (list string)) "keys" [ "alpha"; "gamma" ] (Core_map.keys m2);
  Alcotest.(check (list int)) "values" [ 10; 30 ] (Core_map.values m2);
  let m_mapped = Core_map.map (fun v -> v * 2) m in
  Alcotest.(check (option int))
    "mapped value" (Some 40)
    (Core_map.find_opt "beta" m_mapped)

let test_core_option_and_result () =
  let opt = Core_option.some 42 in
  Alcotest.(check bool) "is_some" true (Core_option.is_some opt);
  Alcotest.(check bool) "is_none" false (Core_option.is_none opt);
  Alcotest.(check int) "value" 42 (Core_option.value ~default:0 opt);
  Alcotest.(check int) "value default" 0 (Core_option.value ~default:0 Core_option.none);
  Alcotest.(check (option int)) "map" (Some 84) (Core_option.map (fun x -> x * 2) opt);
  Alcotest.(check (option int))
    "bind" (Some 84)
    (Core_option.bind opt (fun x -> Some (x * 2)));
  Alcotest.(check int) "fold" 84 (Core_option.fold ~none:0 ~some:(fun x -> x * 2) opt);
  (match Core_option.to_result ~error:"missing" opt with
  | Ok v -> Alcotest.(check int) "to_result ok" 42 v
  | Error _ -> Alcotest.fail "Expected to_result ok");
  (match Core_option.to_result ~error:"missing" Core_option.none with
  | Error err -> Alcotest.(check string) "to_result error" "missing" err
  | Ok _ -> Alcotest.fail "Expected to_result error");

  let res_ok = Core_result.ok 100 in
  let res_err = Core_result.error "failure" in
  Alcotest.(check bool) "res is_ok" true (Core_result.is_ok res_ok);
  Alcotest.(check bool) "res is_error" true (Core_result.is_error res_err);
  Alcotest.(check int) "res value" 100 (Core_result.value ~default:0 res_ok);
  Alcotest.(check int) "res value default" 0 (Core_result.value ~default:0 res_err);
  (match Core_result.map (fun x -> x * 2) res_ok with
  | Ok v -> Alcotest.(check int) "res map" 200 v
  | Error _ -> Alcotest.fail "Expected map ok");
  (match Core_result.map_error (fun s -> "wrapped: " ^ s) res_err with
  | Error e -> Alcotest.(check string) "res map_error" "wrapped: failure" e
  | Ok _ -> Alcotest.fail "Expected map_error error");
  Alcotest.(check (option int))
    "res to_option ok" (Some 100)
    (Core_result.to_option res_ok);
  Alcotest.(check (option int)) "res to_option err" None (Core_result.to_option res_err)

let test_effect_runtime () =
  let env =
    Effect_runtime.create_env ~stdin_str:"line one\nline two\n"
      ~fs_root:"/virtual/sandbox"
      ~vfs:[ ("/virtual/sandbox/test.txt", "hello vfs") ]
      ()
  in
  Effect_runtime.print_string env "hello ";
  Effect_runtime.print_endline env "world";
  Effect_runtime.print_int env 42;
  Effect_runtime.prerr_endline env "warning message";
  Alcotest.(check string)
    "stdout buffer" "hello world\n42"
    (Effect_runtime.get_stdout env);
  Alcotest.(check string)
    "stderr buffer" "warning message\n"
    (Effect_runtime.get_stderr env);

  (match Effect_runtime.read_line env with
  | Ok l1 -> Alcotest.(check string) "stdin line 1" "line one" l1
  | Error diag -> Alcotest.fail diag.message);
  (match Effect_runtime.read_line env with
  | Ok l2 -> Alcotest.(check string) "stdin line 2" "line two" l2
  | Error diag -> Alcotest.fail diag.message);
  (match Effect_runtime.read_line env with
  | Error diag -> Alcotest.(check string) "stdin EOF" "End of file on stdin" diag.message
  | Ok _ -> Alcotest.fail "Expected EOF");

  (match Effect_runtime.read_file env "/virtual/sandbox/test.txt" with
  | Ok content -> Alcotest.(check string) "read vfs file" "hello vfs" content
  | Error diag -> Alcotest.fail diag.message);

  (match Effect_runtime.write_file env "/virtual/sandbox/new.txt" "new content" with
  | Ok () -> ()
  | Error diag -> Alcotest.fail diag.message);
  Alcotest.(check bool)
    "file_exists" true
    (Effect_runtime.file_exists env "/virtual/sandbox/new.txt");
  (match Effect_runtime.read_file env "/virtual/sandbox/new.txt" with
  | Ok c -> Alcotest.(check string) "read written file" "new content" c
  | Error diag -> Alcotest.fail diag.message);

  (* Traversal attack test *)
  (match Effect_runtime.read_file env "/virtual/sandbox/../../etc/passwd" with
  | Error diag ->
      Alcotest.(check bool)
        "traversal blocked" true
        (String.length diag.message > 0 && diag.code = Error_code.E0001_io_error)
  | Ok _ -> Alcotest.fail "Expected directory traversal error");

  Effect_runtime.clear_buffers env;
  Alcotest.(check string) "cleared stdout" "" (Effect_runtime.get_stdout env);
  Alcotest.(check string) "cleared stderr" "" (Effect_runtime.get_stderr env)

let test_prelude () =
  let ast = Prelude.prelude_ast () in
  Alcotest.(check bool) "prelude has decls" true (List.length ast.decls > 0);
  let core = Prelude.prelude_core () in
  Alcotest.(check bool) "prelude core has decls" true (List.length core.decls > 0);
  let type_env = Prelude.default_type_env () in
  Alcotest.(check bool) "type_env has abs" true (Type_env.mem "abs" type_env);
  Alcotest.(check bool) "type_env has min" true (Type_env.mem "min" type_env);
  Alcotest.(check bool) "type_env has max" true (Type_env.mem "max" type_env);
  Alcotest.(check bool) "type_env has clamp" true (Type_env.mem "clamp" type_env);
  Alcotest.(check bool) "type_env has id" true (Type_env.mem "id" type_env);
  Alcotest.(check bool) "type_env has compose" true (Type_env.mem "compose" type_env);

  let eval_env = Prelude.default_eval_env () in
  Alcotest.(check bool) "eval_env has abs" true (Env.mem "abs" eval_env);
  Alcotest.(check bool) "eval_env has min" true (Env.mem "min" eval_env);

  (* Evaluate a user expression utilizing prelude functions with tree-walker *)
  let run_with_prelude_eval src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag.message
    | Ok prog -> (
        match Desugar.lower prog with
        | Error err -> Error err.message
        | Ok core_prog -> (
            match Eval.eval_program ~env:eval_env core_prog with
            | Ok v -> Ok v
            | Error err -> Error err.message))
  in
  (match run_with_prelude_eval "abs (-99);" with
  | Ok (Value.VInt 99) -> ()
  | _ -> Alcotest.fail "Expected abs (-99) = 99");
  (match run_with_prelude_eval "min 10 20;" with
  | Ok (Value.VInt 10) -> ()
  | _ -> Alcotest.fail "Expected min 10 20 = 10");
  (match run_with_prelude_eval "clamp 0 100 150;" with
  | Ok (Value.VInt 100) -> ()
  | _ -> Alcotest.fail "Expected clamp 0 100 150 = 100");

  (* Compile with prelude to VM and run *)
  let run_with_prelude_vm src =
    match Parse_facade.parse_string ~file:"<test>" src with
    | Error diag -> Error diag.message
    | Ok prog -> (
        let prog_with_prelude = Prelude.with_prelude prog in
        match Compiler.compile prog_with_prelude with
        | Error err -> Error err.message
        | Ok chunk -> (
            match Vm.run_outcome_ast_error chunk with
            | Ok out -> Ok out.value
            | Error err -> Error err.message))
  in
  (match run_with_prelude_vm "abs (-99);" with
  | Ok (Value.VInt 99) -> ()
  | _ -> Alcotest.fail "Expected VM abs (-99) = 99");
  (match run_with_prelude_vm "max 30 15;" with
  | Ok (Value.VInt 30) -> ()
  | _ -> Alcotest.fail "Expected VM max 30 15 = 30");
  match run_with_prelude_vm "compose succ abs (-5);" with
  | Ok (Value.VInt 6) -> ()
  | _ -> Alcotest.fail "Expected VM compose succ abs (-5) = 6"

let () =
  let open Alcotest in
  run "Vesper Standard Library Unit Tests"
    [
      ("Core_math", [ test_case "Math and checked arithmetic" `Quick test_core_math ]);
      ("Core_string", [ test_case "String manipulation" `Quick test_core_string ]);
      ("Core_list", [ test_case "Functional list operations" `Quick test_core_list ]);
      ("Core_map", [ test_case "Immutable string map" `Quick test_core_map ]);
      ( "Core_option & Result",
        [ test_case "Monadic option and result" `Quick test_core_option_and_result ] );
      ( "Effect_runtime",
        [ test_case "Isolated I/O, VFS, and security sandbox" `Quick test_effect_runtime ]
      );
      ( "Prelude",
        [ test_case "Standard prelude ingestion and execution" `Quick test_prelude ] );
    ]
