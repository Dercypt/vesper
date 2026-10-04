module StringMap = Map.Make (String)

type checked_module = {
  name : string;
  file_path : string;
  namespace : Namespace.t;
  public_namespace : Namespace.t;
  interface : Mod_ast.mod_sig option;
  span : Ast.span;
}

type checked_project = {
  modules : checked_module list;
  build_order : string list;
  root_env : Namespace.t;
}

let rec check_interface_conformance ~(impl_ns : Namespace.t)
    ~(interface : Mod_ast.mod_sig) : (unit, Diagnostic.t list) result =
  let diags = ref [] in
  let var_counter = ref 1000 in
  let fresh () =
    let v = !var_counter in
    incr var_counter;
    Types.TyVar v
  in
  let rec check_items = function
    | [] -> ()
    | (it : Mod_ast.sig_item) :: rest ->
        (match it with
        | SigVal spec -> (
            match Namespace.lookup_val_direct spec.name impl_ns with
            | None ->
                let msg =
                  Printf.sprintf
                    "Interface conformance mismatch: value '%s' declared in interface is \
                     not implemented"
                    spec.name
                in
                diags :=
                  Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                    ~span:spec.span msg
                  :: !diags
            | Some val_entry -> (
                let inst_ty = Type_env.instantiate fresh val_entry.scheme in
                match Unify.unify ~span:spec.span inst_ty spec.ty with
                | Ok _ -> ()
                | Error _ ->
                    let msg =
                      Printf.sprintf
                        "Interface conformance mismatch for '%s': interface declares \
                         '%s', but implementation provides '%s'"
                        spec.name (Types.to_string spec.ty)
                        (Types.scheme_to_string val_entry.scheme)
                    in
                    diags :=
                      Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                        ~span:spec.span msg
                      :: !diags))
        | SigType spec -> (
            match Namespace.lookup_type_direct spec.name impl_ns with
            | None ->
                let msg =
                  Printf.sprintf
                    "Interface conformance mismatch: type '%s' declared in interface is \
                     not defined in implementation"
                    spec.name
                in
                diags :=
                  Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                    ~span:spec.span msg
                  :: !diags
            | Some type_entry -> (
                match spec.manifest with
                | Some iface_m -> (
                    match type_entry.manifest with
                    | Some impl_m ->
                        if not (Types.equal iface_m impl_m) then
                          let msg =
                            Printf.sprintf
                              "Interface conformance mismatch for type '%s': interface \
                               declares '%s', but implementation has '%s'"
                              spec.name (Types.to_string iface_m) (Types.to_string impl_m)
                          in
                          diags :=
                            Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                              ~span:spec.span msg
                            :: !diags
                    | None ->
                        let msg =
                          Printf.sprintf
                            "Interface conformance mismatch for type '%s': interface \
                             requires manifest '%s', but type is abstract in \
                             implementation"
                            spec.name (Types.to_string iface_m)
                        in
                        diags :=
                          Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                            ~span:spec.span msg
                          :: !diags)
                | None -> ()))
        | SigModule { name; signature = sub_sig; span } -> (
            match Namespace.lookup_module_direct name impl_ns with
            | None ->
                let msg =
                  Printf.sprintf
                    "Interface conformance mismatch: submodule '%s' is not implemented"
                    name
                in
                diags :=
                  Diagnostic.error ~code:Error_code.E4003_interface_mismatch ~span msg
                  :: !diags
            | Some sub_mod -> (
                match
                  check_interface_conformance ~impl_ns:sub_mod.namespace
                    ~interface:sub_sig
                with
                | Ok () -> ()
                | Error sub_diags -> diags := sub_diags @ !diags))
        | SigOpen _ -> ());
        check_items rest
  in
  match interface with
  | Mod_ast.SigBody { items; _ } ->
      check_items items;
      if !diags = [] then Ok () else Error (List.rev !diags)
  | Mod_ast.SigIdent { path; span } -> (
      match Namespace.resolve_module ~path impl_ns with
      | Ok _ -> Ok ()
      | Error err ->
          let diag =
            Diagnostic.error ~code:Error_code.E4003_interface_mismatch ~span
              err.Ast.message
          in
          Error [ diag ])

let check_module ?(project_env = Namespace.empty) ?interface
    (unit : Mod_ast.compilation_unit) : (checked_module, Diagnostic.t list) result =
  let diags = ref [] in
  let rec process_items current_ns = function
    | [] -> current_ns
    | (it : Mod_ast.mod_item) :: rest ->
        let next_ns =
          match it with
          | ModImport { path; alias; span } -> (
              match Namespace.resolve_module ~path current_ns with
              | Ok mod_ent ->
                  let bind_name =
                    match alias with
                    | Some a -> a
                    | None -> (
                        match Mod_ast.last_segment path with
                        | Ok seg -> seg.name
                        | Error _ -> mod_ent.name)
                  in
                  Namespace.bind_module ~name:bind_name ~mod_ns:mod_ent.namespace ~span
                    current_ns
              | Error err ->
                  let diag =
                    Diagnostic.error ~code:Error_code.E4002_unbound_module
                      ~span:err.Ast.span err.Ast.message
                  in
                  diags := diag :: !diags;
                  current_ns)
          | ModOpen { path; span = _ } -> (
              match Namespace.open_module ~path current_ns with
              | Ok opened_ns -> opened_ns
              | Error err ->
                  let diag =
                    Diagnostic.error ~code:Error_code.E4002_unbound_module
                      ~span:err.Ast.span err.Ast.message
                  in
                  diags := diag :: !diags;
                  current_ns)
          | ModVal val_def -> (
              let type_env = Namespace.to_type_env current_ns in
              let decl : Ast.decl =
                {
                  span = val_def.span;
                  decl_desc =
                    Ast.LetDecl
                      {
                        name = val_def.name;
                        is_rec = val_def.is_rec;
                        args = val_def.args;
                        value = val_def.value;
                      };
                }
              in
              match Typecheck.typecheck_decl ~env:type_env decl with
              | Error err ->
                  let diag =
                    Diagnostic.of_ast_error ~code:Error_code.E2001_type_error err
                  in
                  diags := diag :: !diags;
                  current_ns
              | Ok (_, extended_env) ->
                  let scheme =
                    match Type_env.lookup val_def.name extended_env with
                    | Some s -> s
                    | None -> Types.Forall ([], Types.TyUnit)
                  in
                  Namespace.bind_val ~name:val_def.name ~scheme ~span:val_def.span
                    current_ns)
          | ModType type_def ->
              Namespace.bind_type ~name:type_def.name ~params:type_def.params
                ~manifest:type_def.ty ~span:type_def.span current_ns
          | ModModule { name; expr; span } -> (
              match expr with
              | Mod_ast.ModStruct { items = inner_items; _ } ->
                  let inner_checked =
                    process_items
                      (Namespace.open_namespace current_ns ~into:Namespace.empty)
                      inner_items
                  in
                  Namespace.bind_module ~name ~mod_ns:inner_checked ~span current_ns
              | Mod_ast.ModVar { path; _ } -> (
                  match Namespace.resolve_module ~path current_ns with
                  | Ok mod_ent ->
                      Namespace.bind_module ~name ~mod_ns:mod_ent.namespace ~span
                        current_ns
                  | Error err ->
                      let diag =
                        Diagnostic.error ~code:Error_code.E4002_unbound_module
                          ~span:err.Ast.span err.Ast.message
                      in
                      diags := diag :: !diags;
                      current_ns)
              | Mod_ast.ModAscribed { expr = inner_e; signature = inner_sig; _ } -> (
                  match inner_e with
                  | Mod_ast.ModStruct { items = inner_items; _ } -> (
                      let inner_checked =
                        process_items
                          (Namespace.open_namespace current_ns ~into:Namespace.empty)
                          inner_items
                      in
                      match
                        check_interface_conformance ~impl_ns:inner_checked
                          ~interface:inner_sig
                      with
                      | Error conf_diags ->
                          diags := conf_diags @ !diags;
                          current_ns
                      | Ok () -> (
                          match Namespace.restrict_interface inner_sig inner_checked with
                          | Ok restricted ->
                              Namespace.bind_module ~name ~mod_ns:restricted ~span
                                current_ns
                          | Error err ->
                              let diag =
                                Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                                  ~span:err.Ast.span err.Ast.message
                              in
                              diags := diag :: !diags;
                              current_ns))
                  | _ -> current_ns))
        in
        process_items next_ns rest
  in
  let init_ns =
    if Namespace.is_empty project_env then Namespace.empty
    else Namespace.open_namespace project_env ~into:Namespace.empty
  in
  let raw_ns =
    match unit.kind with
    | Implementation (ModStruct { items; _ }) -> process_items init_ns items
    | Implementation (ModVar { path; _ }) -> (
        match Namespace.resolve_module ~path init_ns with
        | Ok mod_ent -> mod_ent.namespace
        | Error err ->
            let diag =
              Diagnostic.error ~code:Error_code.E4002_unbound_module ~span:err.Ast.span
                err.Ast.message
            in
            diags := diag :: !diags;
            init_ns)
    | Implementation (ModAscribed { expr; signature; _ }) -> (
        match expr with
        | ModStruct { items; _ } -> (
            let checked = process_items init_ns items in
            match check_interface_conformance ~impl_ns:checked ~interface:signature with
            | Error conf_diags ->
                diags := conf_diags @ !diags;
                checked
            | Ok () -> (
                match Namespace.restrict_interface signature checked with
                | Ok restricted -> restricted
                | Error err ->
                    let diag =
                      Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                        ~span:err.Ast.span err.Ast.message
                    in
                    diags := diag :: !diags;
                    checked))
        | _ -> init_ns)
    | Interface _ -> init_ns
  in
  if !diags <> [] then Error (List.rev !diags)
  else
    match interface with
    | Some iface -> (
        match check_interface_conformance ~impl_ns:raw_ns ~interface:iface with
        | Error conf_diags -> Error conf_diags
        | Ok () -> (
            match Namespace.restrict_interface iface raw_ns with
            | Error err ->
                let diag =
                  Diagnostic.error ~code:Error_code.E4003_interface_mismatch
                    ~span:err.Ast.span err.Ast.message
                in
                Error [ diag ]
            | Ok restricted_ns ->
                let pub_ns = Namespace.export_public_only restricted_ns in
                Ok
                  {
                    name = unit.name;
                    file_path = unit.file_path;
                    namespace = restricted_ns;
                    public_namespace = pub_ns;
                    interface = Some iface;
                    span = unit.span;
                  }))
    | None ->
        let pub_ns = Namespace.export_public_only raw_ns in
        Ok
          {
            name = unit.name;
            file_path = unit.file_path;
            namespace = raw_ns;
            public_namespace = pub_ns;
            interface = None;
            span = unit.span;
          }

let check_project (units : Mod_ast.compilation_unit list) :
    (checked_project, Diagnostic.t list) result =
  let interfaces = ref StringMap.empty in
  let implementations = ref [] in
  List.iter
    (fun (u : Mod_ast.compilation_unit) ->
      match u.kind with
      | Interface sign -> interfaces := StringMap.add u.name sign !interfaces
      | Implementation _ -> implementations := u :: !implementations)
    units;

  let impl_units = List.rev !implementations in
  match Dep_graph.of_units impl_units with
  | Error err ->
      let diag =
        Diagnostic.error ~code:Error_code.E0002_internal_error ~span:err.Ast.span
          err.Ast.message
      in
      Error [ diag ]
  | Ok dep_graph -> (
      match Dep_graph.detect_cycles dep_graph with
      | Error err ->
          let diag =
            Diagnostic.error ~code:Error_code.E4001_cyclic_dependency ~span:err.Ast.span
              err.Ast.message
          in
          Error [ diag ]
      | Ok () -> (
          match Dep_graph.topological_sort dep_graph with
          | Error err ->
              let diag =
                Diagnostic.error ~code:Error_code.E4001_cyclic_dependency
                  ~span:err.Ast.span err.Ast.message
              in
              Error [ diag ]
          | Ok build_order ->
              let project_env = ref Namespace.empty in
              let checked_modules = ref [] in
              let all_diags = ref [] in
              List.iter
                (fun mod_name ->
                  match
                    List.find_opt
                      (fun (u : Mod_ast.compilation_unit) -> u.name = mod_name)
                      impl_units
                  with
                  | None -> ()
                  | Some unit -> (
                      let iface_opt = StringMap.find_opt mod_name !interfaces in
                      match
                        check_module ~project_env:!project_env ?interface:iface_opt unit
                      with
                      | Error diags -> all_diags := diags @ !all_diags
                      | Ok checked_m ->
                          checked_modules := checked_m :: !checked_modules;
                          project_env :=
                            Namespace.bind_module ~name:checked_m.name
                              ~mod_ns:checked_m.public_namespace ~span:checked_m.span
                              !project_env))
                build_order;

              if !all_diags <> [] then Error (List.rev !all_diags)
              else
                Ok
                  {
                    modules = List.rev !checked_modules;
                    build_order;
                    root_env = !project_env;
                  }))

let compile_files (file_paths : string list) : (checked_project, Diagnostic.t list) result
    =
  let rec parse_all acc = function
    | [] -> Ok (List.rev acc)
    | file :: rest -> (
        let dummy_span =
          match
            Ast.create_span ~file ~start_line:1 ~start_col:1 ~end_line:1 ~end_col:1
          with
          | Ok s -> s
          | Error _ -> Ast.dummy_span
        in
        try
          let ic = open_in file in
          let len = in_channel_length ic in
          let content = really_input_string ic len in
          close_in ic;
          match Mod_ast.parse_compilation_unit ~file content with
          | Ok cu -> parse_all (cu :: acc) rest
          | Error err ->
              let diag =
                Diagnostic.error ~code:Error_code.E1008_syntax_error ~span:err.Ast.span
                  err.Ast.message
              in
              Error [ diag ]
        with
        | Sys_error msg ->
            let diag =
              Diagnostic.error ~code:Error_code.E0001_io_error ~span:dummy_span
                (Printf.sprintf "I/O error reading file '%s': %s" file msg)
            in
            Error [ diag ]
        | exn ->
            let diag =
              Diagnostic.error ~code:Error_code.E0002_internal_error ~span:dummy_span
                (Printf.sprintf "Unexpected failure reading '%s': %s" file
                   (Printexc.to_string exn))
            in
            Error [ diag ])
  in
  match parse_all [] file_paths with
  | Ok units -> check_project units
  | Error diags -> Error diags
