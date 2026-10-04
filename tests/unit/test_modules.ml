open Vesper
open Vesper.Mod_ast

let test_span : Ast.span =
  match
    Ast.create_span ~file:"test_modules.vesper" ~start_line:1 ~start_col:1 ~end_line:1
      ~end_col:10
  with
  | Ok s -> s
  | Error _ -> Ast.dummy_span

(** {1 Mini-Goal 7.1: Module AST & Qualified Path Tests} *)

let test_path_operations () =
  let path_res = parse_path ~span:test_span "Math.Vector.add" in
  Alcotest.(check bool) "Parse path succeeded" true (Result.is_ok path_res);
  let path = Result.get_ok path_res in
  Alcotest.(check string) "Path to string" "Math.Vector.add" (path_to_string path);
  Alcotest.(check int) "3 segments" 3 (List.length path.segments);

  (* Span tracking across all segments (Step 7.1.2) *)
  let s0 = List.nth path.segments 0 in
  let s1 = List.nth path.segments 1 in
  let s2 = List.nth path.segments 2 in
  Alcotest.(check string) "Seg 0 name" "Math" s0.name;
  Alcotest.(check int) "Seg 0 start col" 1 s0.span.start_col;
  Alcotest.(check int) "Seg 0 end col" 5 s0.span.end_col;
  Alcotest.(check string) "Seg 1 name" "Vector" s1.name;
  Alcotest.(check int) "Seg 1 start col" 6 s1.span.start_col;
  Alcotest.(check int) "Seg 1 end col" 12 s1.span.end_col;
  Alcotest.(check string) "Seg 2 name" "add" s2.name;
  Alcotest.(check int) "Seg 2 start col" 13 s2.span.start_col;
  Alcotest.(check int) "Seg 2 end col" 16 s2.span.end_col;

  (* Head, last, and drop_last *)
  let head = Result.get_ok (head_segment path) in
  Alcotest.(check string) "Head segment" "Math" head.name;
  let last = Result.get_ok (last_segment path) in
  Alcotest.(check string) "Last segment" "add" last.name;
  let prefix, dropped = Result.get_ok (drop_last path) in
  Alcotest.(check string) "Dropped seg" "add" dropped.name;
  Alcotest.(check string) "Prefix path" "Math.Vector" (path_to_string prefix);

  (* Empty path error handling (Law 1) *)
  let empty_parse = parse_path ~span:test_span "" in
  Alcotest.(check bool) "Empty path rejected" true (Result.is_error empty_parse);
  let dot_parse = parse_path ~span:test_span "A..B" in
  Alcotest.(check bool) "Invalid dots rejected" true (Result.is_error dot_parse)

let test_ast_validation_and_laws () =
  let path = Result.get_ok (parse_path ~span:test_span "Foo.Bar") in
  let val_spec = make_val_spec ~span:test_span ~name:"calc" ~ty:Types.TyInt in
  let sig_item = SigVal val_spec in
  let signature = make_sig_body ~span:test_span [ sig_item ] in
  let dummy_expr = { Ast.span = test_span; desc = Ast.Lit (Ast.Int 42) } in
  let val_def = make_val_def ~span:test_span ~name:"x" dummy_expr in
  let mod_item = ModVal val_def in
  let mod_expr = make_mod_struct ~span:test_span [ mod_item ] in
  let unit =
    make_unit ~span:test_span ~name:"TestMod" ~file_path:"test.vesper"
      (Implementation mod_expr)
  in

  (* Law 3: provenance checks *)
  Alcotest.(check bool) "Validate path" true (Result.is_ok (validate_path path));
  Alcotest.(check bool) "Validate sig" true (Result.is_ok (validate_sig signature));
  Alcotest.(check bool)
    "Validate mod expr" true
    (Result.is_ok (validate_mod_expr mod_expr));
  Alcotest.(check bool) "Validate unit" true (Result.is_ok (validate_unit unit));

  (* Law 2: structural equality modulo spans *)
  let dummy_span2 = Ast.dummy_span in
  let path2 = Result.get_ok (parse_path ~span:dummy_span2 "Foo.Bar") in
  Alcotest.(check bool) "Path equal modulo spans" true (path_equal path path2);

  let mod_expr2 =
    make_mod_struct ~span:dummy_span2
      [
        ModVal
          (make_val_def ~span:dummy_span2 ~name:"x"
             { Ast.span = dummy_span2; desc = Ast.Lit (Ast.Int 42) });
      ]
  in
  Alcotest.(check bool)
    "Mod expr equal modulo spans" true
    (mod_expr_equal mod_expr mod_expr2)

(** {1 Mini-Goal 7.2: Import Scanner, Dep Graph, Cycle Detection & Topological Sort} *)

let test_import_scanner () =
  let code =
    "// Header comment\n\
     import Math\n\
     open Prelude\n\
     import Data.Vector as Vec\n\
     from Collections import List\n\
     let x = 1\n"
  in
  let imports =
    Result.get_ok (Dep_graph.scan_imports_from_string ~file:"test.vesper" code)
  in
  let mod_names = List.map (fun (i : Dep_graph.import_info) -> i.module_name) imports in
  Alcotest.(check (list string))
    "Extracted imports"
    [ "Math"; "Prelude"; "Data"; "Collections" ]
    mod_names

let test_dep_graph_and_cycles () =
  let g0 = Dep_graph.empty in
  let g1 = Dep_graph.add_node ~name:"A" ~span:test_span g0 in
  let g2 = Dep_graph.add_node ~name:"B" ~span:test_span g1 in
  let g3 = Dep_graph.add_node ~name:"C" ~span:test_span g2 in

  (* A -> B, B -> C (Acyclic) *)
  let g_dag =
    Dep_graph.add_dependency ~from_node:"A" ~to_node:"B" ~span:test_span g3
    |> Dep_graph.add_dependency ~from_node:"B" ~to_node:"C" ~span:test_span
  in
  Alcotest.(check bool)
    "DAG has no cycles" true
    (Result.is_ok (Dep_graph.detect_cycles g_dag));

  let sort_res = Dep_graph.topological_sort g_dag in
  Alcotest.(check bool) "Topological sort succeeds" true (Result.is_ok sort_res);
  let order = Result.get_ok sort_res in
  (* Dependencies must precede dependents: C before B, B before A *)
  Alcotest.(check (list string)) "Build order" [ "C"; "B"; "A" ] order;

  (* Add cycle: C -> A (A -> B -> C -> A) *)
  let g_cycle =
    Dep_graph.add_dependency ~from_node:"C" ~to_node:"A" ~span:test_span g_dag
  in
  let cycle_res = Dep_graph.detect_cycles g_cycle in
  Alcotest.(check bool) "Cycle detected" true (Result.is_error cycle_res);
  let err = Result.get_error cycle_res in
  Alcotest.(check bool)
    "Cycle message format (Step 7.2.4)" true
    (String.starts_with ~prefix:"Cyclic dependency detected:" err.message);

  (* Self cycle: X -> X *)
  let g_self =
    Dep_graph.add_node ~name:"X" ~span:test_span Dep_graph.empty
    |> Dep_graph.add_dependency ~from_node:"X" ~to_node:"X" ~span:test_span
  in
  let self_res = Dep_graph.detect_cycles g_self in
  Alcotest.(check bool) "Self cycle detected" true (Result.is_error self_res)

(** {1 Mini-Goal 7.3: Namespace Resolution, Shadowing & Visibility Encapsulation} *)

let test_namespace_resolution () =
  let ns0 = Namespace.empty in
  let val_scheme = Types.Forall ([], Types.TyInt) in
  let ns1 = Namespace.bind_val ~name:"x" ~scheme:val_scheme ~span:test_span ns0 in
  let ns2 = Namespace.bind_val ~name:"y" ~scheme:val_scheme ~span:test_span ns1 in

  (* Submodule Sub inside Root *)
  let sub_ns =
    Namespace.bind_val ~name:"inner" ~scheme:val_scheme ~span:test_span Namespace.empty
    |> Namespace.bind_val ~visibility:Namespace.Private ~name:"hidden" ~scheme:val_scheme
         ~span:test_span
  in
  let root_ns = Namespace.bind_module ~name:"Sub" ~mod_ns:sub_ns ~span:test_span ns2 in

  (* Direct lookups *)
  Alcotest.(check bool)
    "Lookup x direct" true
    (Option.is_some (Namespace.lookup_val_direct "x" root_ns));
  Alcotest.(check bool)
    "Lookup y direct" true
    (Option.is_some (Namespace.lookup_val_direct "y" root_ns));

  (* Qualified path lookup: Sub.inner *)
  let path_sub_inner = Result.get_ok (parse_path ~span:test_span "Sub.inner") in
  let res_inner = Namespace.resolve_val ~path:path_sub_inner root_ns in
  Alcotest.(check bool) "Resolve Sub.inner" true (Result.is_ok res_inner);

  (* Private member access rejection: Sub.hidden (Step 7.3.3) *)
  let path_sub_hidden = Result.get_ok (parse_path ~span:test_span "Sub.hidden") in
  let res_hidden = Namespace.resolve_val ~path:path_sub_hidden root_ns in
  Alcotest.(check bool) "Private member access rejected" true (Result.is_error res_hidden);
  let priv_err = Result.get_error res_hidden in
  Alcotest.(check bool)
    "Private member message" true
    (String.starts_with ~prefix:"Private member access:" priv_err.message);

  (* Unbound lookup *)
  let path_unbound = Result.get_ok (parse_path ~span:test_span "Sub.missing") in
  let res_unbound = Namespace.resolve_val ~path:path_unbound root_ns in
  Alcotest.(check bool) "Unbound member rejected" true (Result.is_error res_unbound)

let test_open_and_shadowing () =
  (* Module M1 with a=10, b=20 *)
  let scheme1 = Types.Forall ([], Types.TyInt) in
  let scheme2 = Types.Forall ([], Types.TyString) in
  let m1 =
    Namespace.bind_val ~name:"a" ~scheme:scheme1 ~span:test_span Namespace.empty
    |> Namespace.bind_val ~name:"b" ~scheme:scheme1 ~span:test_span
  in
  (* Module M2 with b="hello", c=30 *)
  let m2 =
    Namespace.bind_val ~name:"b" ~scheme:scheme2 ~span:test_span Namespace.empty
    |> Namespace.bind_val ~name:"c" ~scheme:scheme1 ~span:test_span
  in

  (* Main scope with local a=999, open M1, then open M2 *)
  let main_ns =
    Namespace.bind_module ~name:"M1" ~mod_ns:m1 ~span:test_span Namespace.empty
    |> Namespace.bind_module ~name:"M2" ~mod_ns:m2 ~span:test_span
    |> Namespace.bind_val ~name:"a" ~scheme:scheme1 ~span:test_span
  in

  let path_m1 = Result.get_ok (parse_path ~span:test_span "M1") in
  let path_m2 = Result.get_ok (parse_path ~span:test_span "M2") in
  let ns_opened =
    Namespace.open_module ~path:path_m1 main_ns
    |> Result.get_ok
    |> Namespace.open_module ~path:path_m2
    |> Result.get_ok
  in

  (* Local definition 'a' takes precedence over M1.a *)
  let val_a = Option.get (Namespace.lookup_val "a" ns_opened) in
  Alcotest.(check bool) "Local 'a' found" true (val_a.name = "a");

  (* M2.b shadows M1.b (Step 7.3.4 deterministic shadowing) *)
  let val_b = Option.get (Namespace.lookup_val "b" ns_opened) in
  Alcotest.(check bool)
    "M2.b shadows M1.b with string type" true
    (Types.equal Types.TyString (match val_b.scheme with Types.Forall (_, ty) -> ty));

  (* M2.c is accessible *)
  Alcotest.(check bool)
    "M2.c accessible" true
    (Option.is_some (Namespace.lookup_val "c" ns_opened))

(** {1 Mini-Goal 7.3 & 7.4: Interface Conformance & Project Compilation} *)

let test_interface_conformance () =
  let scheme_add =
    Types.Forall
      ([], Types.TyArrow (Types.TyInt, Types.TyArrow (Types.TyInt, Types.TyInt)))
  in
  let scheme_pi = Types.Forall ([], Types.TyInt) in
  let scheme_helper = Types.Forall ([], Types.TyInt) in

  let impl_ns =
    Namespace.bind_val ~name:"add" ~scheme:scheme_add ~span:test_span Namespace.empty
    |> Namespace.bind_val ~name:"pi" ~scheme:scheme_pi ~span:test_span
    |> Namespace.bind_val ~name:"secret_helper" ~scheme:scheme_helper ~span:test_span
  in

  (* Matching interface *)
  let iface_val_add =
    make_val_spec ~span:test_span ~name:"add"
      ~ty:(Types.TyArrow (Types.TyInt, Types.TyArrow (Types.TyInt, Types.TyInt)))
  in
  let iface_val_pi = make_val_spec ~span:test_span ~name:"pi" ~ty:Types.TyInt in
  let iface_sig =
    make_sig_body ~span:test_span [ SigVal iface_val_add; SigVal iface_val_pi ]
  in

  let conf_res = Module_check.check_interface_conformance ~impl_ns ~interface:iface_sig in
  Alcotest.(check bool) "Matching interface passes" true (Result.is_ok conf_res);

  (* Restrict interface hides secret_helper *)
  let restricted = Result.get_ok (Namespace.restrict_interface iface_sig impl_ns) in
  let helper_entry =
    Option.get (Namespace.lookup_val_direct "secret_helper" restricted)
  in
  Alcotest.(check bool)
    "secret_helper is private after restriction" true
    (helper_entry.visibility = Namespace.Private);

  (* Mismatched type interface *)
  let bad_pi_spec = make_val_spec ~span:test_span ~name:"pi" ~ty:Types.TyBool in
  let bad_sig = make_sig_body ~span:test_span [ SigVal bad_pi_spec ] in
  let bad_conf = Module_check.check_interface_conformance ~impl_ns ~interface:bad_sig in
  Alcotest.(check bool) "Type mismatch detected" true (Result.is_error bad_conf);
  let diags = Result.get_error bad_conf in
  let d0 = List.hd diags in
  Alcotest.(check bool)
    "E4003 error code emitted" true
    (Error_code.equal d0.Diagnostic.code Error_code.E4003_interface_mismatch);

  (* Missing item interface *)
  let missing_spec = make_val_spec ~span:test_span ~name:"missing" ~ty:Types.TyInt in
  let missing_sig = make_sig_body ~span:test_span [ SigVal missing_spec ] in
  let missing_conf =
    Module_check.check_interface_conformance ~impl_ns ~interface:missing_sig
  in
  Alcotest.(check bool) "Missing value detected" true (Result.is_error missing_conf)

let find_fixture_dir () =
  if Sys.file_exists "tests/fixtures/modules/math.vesperi" then "tests/fixtures/modules"
  else if Sys.file_exists "../fixtures/modules/math.vesperi" then "../fixtures/modules"
  else "../../fixtures/modules"

let test_fixtures_multi_module_project () =
  let fixture_dir = find_fixture_dir () in
  let math_iface = Filename.concat fixture_dir "math.vesperi" in
  let math_impl = Filename.concat fixture_dir "math.vesper" in
  let utils_impl = Filename.concat fixture_dir "utils.vesper" in

  let res = Module_check.compile_files [ math_iface; math_impl; utils_impl ] in
  Alcotest.(check bool) "Compile multi-module project succeeds" true (Result.is_ok res);
  let project = Result.get_ok res in
  Alcotest.(check (list string)) "Build order" [ "Math"; "Utils" ] project.build_order;

  (* Verify Math exports are in root_env *)
  let math_mod = Option.get (Namespace.lookup_module_direct "Math" project.root_env) in
  Alcotest.(check bool)
    "Math.add is public" true
    (match Namespace.lookup_val_direct "add" math_mod.namespace with
    | Some e -> e.visibility = Namespace.Public
    | None -> false);
  Alcotest.(check bool)
    "Math.pi is public" true
    (match Namespace.lookup_val_direct "pi" math_mod.namespace with
    | Some e -> e.visibility = Namespace.Public
    | None -> false);
  Alcotest.(check bool)
    "Math.secret_offset is private/hidden" true
    (match Namespace.lookup_val_direct "secret_offset" math_mod.namespace with
    | Some e -> e.visibility = Namespace.Private
    | None -> true)

let test_fixture_cycle_detection () =
  let fixture_dir = find_fixture_dir () in
  let cycle_a = Filename.concat fixture_dir "cycle_a.vesper" in
  let cycle_b = Filename.concat fixture_dir "cycle_b.vesper" in

  let res = Module_check.compile_files [ cycle_a; cycle_b ] in
  Alcotest.(check bool) "Cycles in fixtures detected" true (Result.is_error res);
  let diags = Result.get_error res in
  let d0 = List.hd diags in
  Alcotest.(check bool)
    "Cycle diagnostic has E4001" true
    (Error_code.equal d0.Diagnostic.code Error_code.E4001_cyclic_dependency)

let test_fixture_bad_interface () =
  let fixture_dir = find_fixture_dir () in
  let bad_iface = Filename.concat fixture_dir "bad_module.vesperi" in
  let bad_impl = Filename.concat fixture_dir "bad_module.vesper" in

  let res = Module_check.compile_files [ bad_iface; bad_impl ] in
  Alcotest.(check bool) "Bad interface produces diagnostics" true (Result.is_error res);
  let diags = Result.get_error res in
  Alcotest.(check bool)
    "Diagnostics contain E4003" true
    (List.exists
       (fun (d : Diagnostic.t) ->
         Error_code.equal d.code Error_code.E4003_interface_mismatch)
       diags)

let () =
  Alcotest.run "Vesper Module System Unit Tests"
    [
      ( "Module AST & Path Operations",
        [
          Alcotest.test_case "Qualified path lookups and spans" `Quick
            test_path_operations;
          Alcotest.test_case "AST validation and laws" `Quick test_ast_validation_and_laws;
        ] );
      ( "Dependency Graph & Cycles",
        [
          Alcotest.test_case "Fast import scanner" `Quick test_import_scanner;
          Alcotest.test_case "DAG & Tarjan cycle detection" `Quick
            test_dep_graph_and_cycles;
        ] );
      ( "Hierarchical Namespaces & Visibility",
        [
          Alcotest.test_case "Namespace resolution & private protection" `Quick
            test_namespace_resolution;
          Alcotest.test_case "Open shadowing rules" `Quick test_open_and_shadowing;
        ] );
      ( "Interface Conformance & Project Pipeline",
        [
          Alcotest.test_case "Interface conformance checking" `Quick
            test_interface_conformance;
          Alcotest.test_case "Multi-module project compilation" `Quick
            test_fixtures_multi_module_project;
          Alcotest.test_case "Cycle detection on fixtures" `Quick
            test_fixture_cycle_detection;
          Alcotest.test_case "Interface mismatch diagnostics" `Quick
            test_fixture_bad_interface;
        ] );
    ]
