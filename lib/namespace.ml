module StringMap = Map.Make (String)

type visibility = Public | Private

type val_entry = {
  name : string;
  scheme : Types.scheme;
  visibility : visibility;
  span : Ast.span;
}

type type_entry = {
  name : string;
  params : string list;
  manifest : Types.t option;
  visibility : visibility;
  span : Ast.span;
}

type mod_entry = {
  name : string;
  namespace : t;
  visibility : visibility;
  span : Ast.span;
}

and t = {
  values : val_entry StringMap.t;
  types : type_entry StringMap.t;
  modules : mod_entry StringMap.t;
  opened_namespaces : t list;
}

let empty : t =
  {
    values = StringMap.empty;
    types = StringMap.empty;
    modules = StringMap.empty;
    opened_namespaces = [];
  }

let is_empty (ns : t) : bool =
  StringMap.is_empty ns.values && StringMap.is_empty ns.types
  && StringMap.is_empty ns.modules && ns.opened_namespaces = []

let bind_val ?(visibility = Public) ~name ~scheme ~span (ns : t) : t =
  let entry = { name; scheme; visibility; span } in
  { ns with values = StringMap.add name entry ns.values }

let bind_type ?(visibility = Public) ~name ?(params = []) ?manifest ~span (ns : t) : t =
  let entry = { name; params; manifest; visibility; span } in
  { ns with types = StringMap.add name entry ns.types }

let bind_module ?(visibility = Public) ~name ~mod_ns ~span (ns : t) : t =
  let entry = { name; namespace = mod_ns; visibility; span } in
  { ns with modules = StringMap.add name entry ns.modules }

(** {1 Direct & Lexical Lookups} *)

let lookup_val_direct name (ns : t) = StringMap.find_opt name ns.values
let lookup_type_direct name (ns : t) = StringMap.find_opt name ns.types
let lookup_module_direct name (ns : t) = StringMap.find_opt name ns.modules

let lookup_val name (ns : t) : val_entry option =
  match lookup_val_direct name ns with
  | Some entry -> Some entry
  | None ->
      let rec search_opened = function
        | [] -> None
        | op :: rest -> (
            match lookup_val_direct name op with
            | Some entry when entry.visibility = Public -> Some entry
            | _ -> ( match search_opened rest with Some e -> Some e | None -> None))
      in
      search_opened ns.opened_namespaces

let lookup_type name (ns : t) : type_entry option =
  match lookup_type_direct name ns with
  | Some entry -> Some entry
  | None ->
      let rec search_opened = function
        | [] -> None
        | op :: rest -> (
            match lookup_type_direct name op with
            | Some entry when entry.visibility = Public -> Some entry
            | _ -> ( match search_opened rest with Some e -> Some e | None -> None))
      in
      search_opened ns.opened_namespaces

let lookup_module name (ns : t) : mod_entry option =
  match lookup_module_direct name ns with
  | Some entry -> Some entry
  | None ->
      let rec search_opened = function
        | [] -> None
        | op :: rest -> (
            match lookup_module_direct name op with
            | Some entry when entry.visibility = Public -> Some entry
            | _ -> ( match search_opened rest with Some e -> Some e | None -> None))
      in
      search_opened ns.opened_namespaces

(** {1 Qualified Path Resolution (Mini-Goal 7.3, Step 7.3.1 & 7.3.3)} *)

let resolve_module ~(path : Mod_ast.path) (root_ns : t) : (mod_entry, Ast.error) result =
  match path.segments with
  | [] ->
      Error
        { Ast.span = path.span; message = "Cannot resolve empty module identifier path" }
  | [ (single : Mod_ast.path_segment) ] -> (
      match lookup_module single.name root_ns with
      | Some entry -> Ok entry
      | None ->
          Error
            {
              Ast.span = single.span;
              message = Printf.sprintf "Unbound module identifier '%s'" single.name;
            })
  | (first : Mod_ast.path_segment) :: rest -> (
      match lookup_module first.name root_ns with
      | None ->
          Error
            {
              Ast.span = first.span;
              message = Printf.sprintf "Unbound module identifier '%s'" first.name;
            }
      | Some first_mod ->
          let rec follow_modules cur_mod = function
            | [] -> Ok cur_mod
            | [ (last : Mod_ast.path_segment) ] -> (
                match lookup_module_direct last.name cur_mod.namespace with
                | None ->
                    Error
                      {
                        Ast.span = last.span;
                        message =
                          Printf.sprintf "Module '%s' has no submodule '%s'" cur_mod.name
                            last.name;
                      }
                | Some sub_mod ->
                    if sub_mod.visibility = Private then
                      Error
                        {
                          Ast.span = last.span;
                          message =
                            Printf.sprintf
                              "Private member access: submodule '%s' is not exported by \
                               module '%s'"
                              last.name cur_mod.name;
                        }
                    else Ok sub_mod)
            | (seg : Mod_ast.path_segment) :: remaining -> (
                match lookup_module_direct seg.name cur_mod.namespace with
                | None ->
                    Error
                      {
                        Ast.span = seg.span;
                        message =
                          Printf.sprintf "Module '%s' has no submodule '%s'" cur_mod.name
                            seg.name;
                      }
                | Some sub_mod ->
                    if sub_mod.visibility = Private then
                      Error
                        {
                          Ast.span = seg.span;
                          message =
                            Printf.sprintf
                              "Private member access: submodule '%s' is not exported by \
                               module '%s'"
                              seg.name cur_mod.name;
                        }
                    else follow_modules sub_mod remaining)
          in
          follow_modules first_mod rest)

let resolve_val ~(path : Mod_ast.path) (root_ns : t) : (val_entry, Ast.error) result =
  match path.segments with
  | [] ->
      Error
        { Ast.span = path.span; message = "Cannot resolve empty value identifier path" }
  | [ (single : Mod_ast.path_segment) ] -> (
      match lookup_val single.name root_ns with
      | Some entry -> Ok entry
      | None ->
          Error
            {
              Ast.span = single.span;
              message = Printf.sprintf "Unbound value identifier '%s'" single.name;
            })
  | _ -> (
      match Mod_ast.drop_last path with
      | Error err -> Error err
      | Ok (prefix_path, (last_seg : Mod_ast.path_segment)) -> (
          match resolve_module ~path:prefix_path root_ns with
          | Error err -> Error err
          | Ok mod_entry -> (
              match lookup_val_direct last_seg.name mod_entry.namespace with
              | None ->
                  Error
                    {
                      Ast.span = last_seg.span;
                      message =
                        Printf.sprintf "Module '%s' has no member '%s'" mod_entry.name
                          last_seg.name;
                    }
              | Some val_ent ->
                  if val_ent.visibility = Private then
                    Error
                      {
                        Ast.span = last_seg.span;
                        message =
                          Printf.sprintf
                            "Private member access: '%s' is not exported by module '%s'"
                            last_seg.name mod_entry.name;
                      }
                  else Ok val_ent)))

let resolve_type ~(path : Mod_ast.path) (root_ns : t) : (type_entry, Ast.error) result =
  match path.segments with
  | [] ->
      Error
        { Ast.span = path.span; message = "Cannot resolve empty type identifier path" }
  | [ (single : Mod_ast.path_segment) ] -> (
      match lookup_type single.name root_ns with
      | Some entry -> Ok entry
      | None ->
          Error
            {
              Ast.span = single.span;
              message = Printf.sprintf "Unbound type identifier '%s'" single.name;
            })
  | _ -> (
      match Mod_ast.drop_last path with
      | Error err -> Error err
      | Ok (prefix_path, (last_seg : Mod_ast.path_segment)) -> (
          match resolve_module ~path:prefix_path root_ns with
          | Error err -> Error err
          | Ok mod_entry -> (
              match lookup_type_direct last_seg.name mod_entry.namespace with
              | None ->
                  Error
                    {
                      Ast.span = last_seg.span;
                      message =
                        Printf.sprintf "Module '%s' has no type '%s'" mod_entry.name
                          last_seg.name;
                    }
              | Some type_ent ->
                  if type_ent.visibility = Private then
                    Error
                      {
                        Ast.span = last_seg.span;
                        message =
                          Printf.sprintf
                            "Private member access: type '%s' is not exported by module \
                             '%s'"
                            last_seg.name mod_entry.name;
                      }
                  else Ok type_ent)))

(** {1 Open & Shadowing Rules (Mini-Goal 7.3, Step 7.3.4)} *)

let open_module ~(path : Mod_ast.path) (ns : t) : (t, Ast.error) result =
  match resolve_module ~path ns with
  | Ok mod_ent ->
      Ok { ns with opened_namespaces = mod_ent.namespace :: ns.opened_namespaces }
  | Error err -> Error err

let open_namespace (src : t) ~(into : t) : t =
  { into with opened_namespaces = src :: into.opened_namespaces }

(** {1 Encapsulation & Interface Filtering (Mini-Goal 7.3, Step 7.3.2 & 7.3.3)} *)

let rec export_public_only (ns : t) : t =
  let pub_vals =
    StringMap.filter (fun _ (v : val_entry) -> v.visibility = Public) ns.values
  in
  let pub_types =
    StringMap.filter (fun _ (t : type_entry) -> t.visibility = Public) ns.types
  in
  let pub_mods =
    StringMap.filter (fun _ (m : mod_entry) -> m.visibility = Public) ns.modules
    |> StringMap.map (fun (m : mod_entry) ->
        { m with namespace = export_public_only m.namespace })
  in
  { values = pub_vals; types = pub_types; modules = pub_mods; opened_namespaces = [] }

let rec restrict_interface (signature : Mod_ast.mod_sig) (ns : t) : (t, Ast.error) result
    =
  match signature with
  | Mod_ast.SigIdent { path; _ } -> (
      match resolve_module ~path ns with
      | Ok mod_ent -> Ok (export_public_only mod_ent.namespace)
      | Error err -> Error err)
  | Mod_ast.SigBody { items; _ } -> (
      let rec check_items val_names type_names mod_names = function
        | [] -> Ok (val_names, type_names, mod_names)
        | (it : Mod_ast.sig_item) :: rest -> (
            match it with
            | SigVal spec ->
                if not (StringMap.mem spec.name ns.values) then
                  Error
                    {
                      Ast.span = spec.span;
                      message =
                        Printf.sprintf
                          "Interface conformance error: required value '%s' is not \
                           implemented"
                          spec.name;
                    }
                else
                  check_items
                    (StringMap.add spec.name spec.span val_names)
                    type_names mod_names rest
            | SigType spec ->
                if not (StringMap.mem spec.name ns.types) then
                  Error
                    {
                      Ast.span = spec.span;
                      message =
                        Printf.sprintf
                          "Interface conformance error: required type '%s' is not \
                           implemented"
                          spec.name;
                    }
                else
                  check_items val_names
                    (StringMap.add spec.name spec.span type_names)
                    mod_names rest
            | SigModule { name; signature = sub_sig; span } -> (
                match StringMap.find_opt name ns.modules with
                | None ->
                    Error
                      {
                        Ast.span;
                        message =
                          Printf.sprintf
                            "Interface conformance error: required submodule '%s' is not \
                             implemented"
                            name;
                      }
                | Some _ ->
                    check_items val_names type_names
                      (StringMap.add name (sub_sig, span) mod_names)
                      rest)
            | SigOpen { path; _ } -> (
                match open_module ~path ns with
                | Ok _ -> check_items val_names type_names mod_names rest
                | Error err -> Error err))
      in
      match check_items StringMap.empty StringMap.empty StringMap.empty items with
      | Error err -> Error err
      | Ok (val_names, type_names, mod_names) -> (
          let new_vals =
            StringMap.mapi
              (fun k (v : val_entry) ->
                let vis = if StringMap.mem k val_names then Public else Private in
                { v with visibility = vis })
              ns.values
          in
          let new_types =
            StringMap.mapi
              (fun k (t : type_entry) ->
                let vis = if StringMap.mem k type_names then Public else Private in
                { t with visibility = vis })
              ns.types
          in
          let rec restrict_submodules = function
            | [] -> Ok StringMap.empty
            | (k, (v : mod_entry)) :: rest -> (
                match StringMap.find_opt k mod_names with
                | Some (sub_sig, _) -> (
                    match restrict_interface sub_sig v.namespace with
                    | Ok restricted_ns -> (
                        match restrict_submodules rest with
                        | Ok map_rest ->
                            let entry =
                              { v with visibility = Public; namespace = restricted_ns }
                            in
                            Ok (StringMap.add k entry map_rest)
                        | Error err -> Error err)
                    | Error err -> Error err)
                | None -> (
                    match restrict_submodules rest with
                    | Ok map_rest ->
                        let entry = { v with visibility = Private } in
                        Ok (StringMap.add k entry map_rest)
                    | Error err -> Error err))
          in
          match restrict_submodules (StringMap.bindings ns.modules) with
          | Error err -> Error err
          | Ok new_mods ->
              Ok
                {
                  values = new_vals;
                  types = new_types;
                  modules = new_mods;
                  opened_namespaces = ns.opened_namespaces;
                }))

(** {1 Interoperability & Introspection} *)

let to_type_env (ns : t) : Type_env.t =
  let env0 = Type_env.empty in
  let env_opened =
    List.fold_right
      (fun op acc ->
        StringMap.fold
          (fun k (v : val_entry) env ->
            if v.visibility = Public then Type_env.extend k v.scheme env else env)
          op.values acc)
      ns.opened_namespaces env0
  in
  StringMap.fold
    (fun k (v : val_entry) env -> Type_env.extend k v.scheme env)
    ns.values env_opened

let all_vals (ns : t) =
  StringMap.bindings ns.values |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let all_types (ns : t) =
  StringMap.bindings ns.types |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let all_modules (ns : t) =
  StringMap.bindings ns.modules |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let pp fmt (ns : t) =
  Format.fprintf fmt "Namespace {\n";
  StringMap.iter
    (fun k (v : val_entry) ->
      let vis_str = if v.visibility = Public then "pub" else "priv" in
      Format.fprintf fmt "  val %s (%s) : %s\n" k vis_str
        (Types.scheme_to_string v.scheme))
    ns.values;
  StringMap.iter
    (fun k (t : type_entry) ->
      let vis_str = if t.visibility = Public then "pub" else "priv" in
      Format.fprintf fmt "  type %s (%s)\n" k vis_str)
    ns.types;
  StringMap.iter
    (fun k (m : mod_entry) ->
      let vis_str = if m.visibility = Public then "pub" else "priv" in
      Format.fprintf fmt "  module %s (%s)\n" k vis_str)
    ns.modules;
  Format.fprintf fmt "}"

let to_string (ns : t) =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt ns;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
