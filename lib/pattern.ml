let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

type type_decl = {
  name : string;
  params : string list;
  variants : (string * Types.t list) list;
}

type record_decl = {
  name : string;
  params : string list;
  fields : (string * Types.t) list;
}

type constructor_info = {
  name : string;
  type_name : string;
  type_params : string list;
  arg_types : Types.t list;
}

type pattern = { span : Ast.span; desc : desc }

and desc =
  | PWildcard
  | PVar of string
  | PLit of Ast.literal
  | PConstructor of string * pattern list
  | PRecord of (string * pattern) list
  | PTuple of pattern list
  | POr of pattern * pattern
  | PAlias of pattern * string

type clause = {
  span : Ast.span;
  pattern : pattern;
  guard : Core_ir.expr option;
  body : Core_ir.expr;
}

let wildcard ~span = { span; desc = PWildcard }
let var ~span name = { span; desc = PVar name }
let lit ~span l = { span; desc = PLit l }
let int ~span n = { span; desc = PLit (Ast.Int n) }
let bool ~span b = { span; desc = PLit (Ast.Bool b) }
let string ~span s = { span; desc = PLit (Ast.String s) }
let construct ~span name args = { span; desc = PConstructor (name, args) }
let record ~span fields = { span; desc = PRecord fields }
let tuple ~span elems = { span; desc = PTuple elems }
let or_pat ~span p1 p2 = { span; desc = POr (p1, p2) }
let alias ~span p name = { span; desc = PAlias (p, name) }
let make_clause ~span ?guard pattern body = { span; pattern; guard; body }
let make_type_decl ~name ?(params = []) variants = { name; params; variants }
let make_record_decl ~name ?(params = []) fields = { name; params; fields }

let rec bound_vars (p : pattern) : string list =
  match p.desc with
  | PWildcard | PLit _ -> []
  | PVar name -> [ name ]
  | PConstructor (_, args) -> List.concat_map bound_vars args
  | PRecord fields -> List.concat_map (fun (_, pat) -> bound_vars pat) fields
  | PTuple elems -> List.concat_map bound_vars elems
  | POr (p1, _) -> bound_vars p1
  | PAlias (inner, name) -> name :: bound_vars inner

let rec validate (p : pattern) : (unit, Ast.error) result =
  if not (Ast.span_is_valid p.span) then
    Error
      Ast.
        {
          span = p.span;
          message =
            Printf.sprintf "Pattern violates Law 3 with invalid span: %s"
              (Ast.span_to_string p.span);
        }
  else
    match p.desc with
    | PWildcard | PLit _ -> Ok ()
    | PVar name ->
        if String.length name = 0 then
          Error
            Ast.{ span = p.span; message = "Variable pattern identifier cannot be empty" }
        else Ok ()
    | PConstructor (name, args) ->
        if String.length name = 0 then
          Error
            Ast.
              {
                span = p.span;
                message = "Constructor pattern identifier cannot be empty";
              }
        else
          List.fold_left
            (fun acc arg ->
              let* () = acc in
              validate arg)
            (Ok ()) args
    | PRecord fields ->
        if fields = [] then
          Error Ast.{ span = p.span; message = "Record pattern cannot have empty fields" }
        else
          List.fold_left
            (fun acc (field_name, field_pat) ->
              let* () = acc in
              if String.length field_name = 0 then
                Error Ast.{ span = p.span; message = "Record field name cannot be empty" }
              else validate field_pat)
            (Ok ()) fields
    | PTuple elems ->
        if List.length elems < 2 then
          Error
            Ast.
              {
                span = p.span;
                message = "Tuple pattern must contain at least two elements";
              }
        else
          List.fold_left
            (fun acc elem ->
              let* () = acc in
              validate elem)
            (Ok ()) elems
    | POr (p1, p2) ->
        let* () = validate p1 in
        let* () = validate p2 in
        let vars1 = List.sort String.compare (bound_vars p1) in
        let vars2 = List.sort String.compare (bound_vars p2) in
        if not (List.equal String.equal vars1 vars2) then
          Error
            Ast.
              {
                span = p.span;
                message =
                  "Both sides of an or-pattern must bind the exact same set of variables";
              }
        else Ok ()
    | PAlias (inner, name) ->
        if String.length name = 0 then
          Error Ast.{ span = p.span; message = "Alias identifier cannot be empty" }
        else validate inner

let validate_clause (c : clause) : (unit, Ast.error) result =
  if not (Ast.span_is_valid c.span) then
    Error
      Ast.
        {
          span = c.span;
          message =
            Printf.sprintf "Clause violates Law 3 with invalid span: %s"
              (Ast.span_to_string c.span);
        }
  else
    let* () = validate c.pattern in
    let* () = match c.guard with Some g -> Core_ir.validate_expr g | None -> Ok () in
    Core_ir.validate_expr c.body

let validate_type_decl (td : type_decl) : (unit, Ast.error) result =
  if String.length td.name = 0 then
    Error Ast.{ span = Ast.dummy_span; message = "Type declaration name cannot be empty" }
  else if td.variants = [] then
    Error
      Ast.
        {
          span = Ast.dummy_span;
          message = Printf.sprintf "Type '%s' must declare at least one variant" td.name;
        }
  else if List.exists (fun (v, _) -> String.length v = 0) td.variants then
    Error Ast.{ span = Ast.dummy_span; message = "Variant name cannot be empty" }
  else Ok ()

let validate_record_decl (rd : record_decl) : (unit, Ast.error) result =
  if String.length rd.name = 0 then
    Error
      Ast.{ span = Ast.dummy_span; message = "Record declaration name cannot be empty" }
  else if rd.fields = [] then
    Error
      Ast.
        {
          span = Ast.dummy_span;
          message = Printf.sprintf "Record '%s' must declare at least one field" rd.name;
        }
  else if List.exists (fun (f, _) -> String.length f = 0) rd.fields then
    Error Ast.{ span = Ast.dummy_span; message = "Record field name cannot be empty" }
  else Ok ()

let rec equal (p1 : pattern) (p2 : pattern) : bool =
  match (p1.desc, p2.desc) with
  | PWildcard, PWildcard -> true
  | PVar n1, PVar n2 -> String.equal n1 n2
  | PLit l1, PLit l2 -> (
      match (l1, l2) with
      | Ast.Int a, Ast.Int b -> a = b
      | Ast.Bool a, Ast.Bool b -> a = b
      | Ast.String a, Ast.String b -> String.equal a b
      | _ -> false)
  | PConstructor (c1, args1), PConstructor (c2, args2) ->
      String.equal c1 c2 && List.equal equal args1 args2
  | PRecord f1, PRecord f2 ->
      List.equal (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && equal v1 v2) f1 f2
  | PTuple e1, PTuple e2 -> List.equal equal e1 e2
  | POr (a1, b1), POr (a2, b2) -> equal a1 a2 && equal b1 b2
  | PAlias (i1, n1), PAlias (i2, n2) -> equal i1 i2 && String.equal n1 n2
  | _ -> false

let clause_equal (c1 : clause) (c2 : clause) : bool =
  equal c1.pattern c2.pattern
  && (match (c1.guard, c2.guard) with
    | None, None -> true
    | Some g1, Some g2 -> Core_ir.expr_equal g1 g2
    | _ -> false)
  && Core_ir.expr_equal c1.body c2.body

let type_decl_equal (t1 : type_decl) (t2 : type_decl) : bool =
  String.equal t1.name t2.name
  && List.equal String.equal t1.params t2.params
  && List.equal
       (fun (v1, tys1) (v2, tys2) ->
         String.equal v1 v2 && List.equal Types.equal tys1 tys2)
       t1.variants t2.variants

let record_decl_equal (r1 : record_decl) (r2 : record_decl) : bool =
  String.equal r1.name r2.name
  && List.equal String.equal r1.params r2.params
  && List.equal
       (fun (f1, ty1) (f2, ty2) -> String.equal f1 f2 && Types.equal ty1 ty2)
       r1.fields r2.fields

let rec pp fmt p =
  match p.desc with
  | PWildcard -> Format.fprintf fmt "_"
  | PVar name -> Format.fprintf fmt "%s" name
  | PLit (Int n) -> Format.fprintf fmt "%d" n
  | PLit (Bool b) -> Format.fprintf fmt "%b" b
  | PLit (String s) -> Format.fprintf fmt "%S" s
  | PConstructor (name, []) -> Format.fprintf fmt "%s" name
  | PConstructor (name, args) ->
      Format.fprintf fmt "%s(%a)" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        args
  | PRecord fields ->
      Format.fprintf fmt "{ %a }"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (f, pat) -> Format.fprintf fmt "%s = %a" f pp pat))
        fields
  | PTuple elems ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        elems
  | POr (p1, p2) -> Format.fprintf fmt "(%a | %a)" pp p1 pp p2
  | PAlias (inner, name) -> Format.fprintf fmt "(%a as %s)" pp inner name

let to_string p = Format.asprintf "%a" pp p

let pp_type_decl fmt (td : type_decl) =
  let pp_params fmt = function
    | [] -> ()
    | [ p ] -> Format.fprintf fmt "'%s " p
    | ps ->
        Format.fprintf fmt "(%a) "
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt p -> Format.fprintf fmt "'%s" p))
          ps
  in
  let pp_variant fmt (vname, vtypes) =
    if vtypes = [] then Format.fprintf fmt "%s" vname
    else
      Format.fprintf fmt "%s of %a" vname
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ") Types.pp)
        vtypes
  in
  Format.fprintf fmt "type %a%s = %a" pp_params td.params td.name
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ") pp_variant)
    td.variants

let type_decl_to_string (td : type_decl) = Format.asprintf "%a" pp_type_decl td

let pp_record_decl fmt (rd : record_decl) =
  let pp_params fmt = function
    | [] -> ()
    | [ p ] -> Format.fprintf fmt "'%s " p
    | ps ->
        Format.fprintf fmt "(%a) "
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt p -> Format.fprintf fmt "'%s" p))
          ps
  in
  Format.fprintf fmt "type %a%s = { %a }" pp_params rd.params rd.name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (f, ty) -> Format.fprintf fmt "%s : %a" f Types.pp ty))
    rd.fields

let record_decl_to_string (rd : record_decl) = Format.asprintf "%a" pp_record_decl rd

type signature_env = {
  types : (string * type_decl) list;
  records : (string * record_decl) list;
  constructors : (string * constructor_info) list;
}

let empty_sig_env = { types = []; records = []; constructors = [] }

let register_type_decl (td : type_decl) (env : signature_env) : signature_env =
  let new_ctors =
    List.map
      (fun (vname, arg_types) ->
        (vname, { name = vname; type_name = td.name; type_params = td.params; arg_types }))
      td.variants
  in
  let filtered_types =
    List.filter (fun (k, _) -> not (String.equal k td.name)) env.types
  in
  let filtered_ctors =
    List.filter (fun (k, _) -> not (List.mem_assoc k new_ctors)) env.constructors
  in
  {
    env with
    types = (td.name, td) :: filtered_types;
    constructors = new_ctors @ filtered_ctors;
  }

let register_record_decl (rd : record_decl) (env : signature_env) : signature_env =
  let filtered_records =
    List.filter (fun (k, _) -> not (String.equal k rd.name)) env.records
  in
  { env with records = (rd.name, rd) :: filtered_records }

let lookup_type name env = List.assoc_opt name env.types
let lookup_record name env = List.assoc_opt name env.records
let lookup_constructor name env = List.assoc_opt name env.constructors

let constructors_of_type (tname : string) (env : signature_env) :
    (string * int) list option =
  match lookup_type tname env with
  | Some td -> Some (List.map (fun (v, args) -> (v, List.length args)) td.variants)
  | None -> None

let standard_sig_env =
  let bool_td = make_type_decl ~name:"bool" [ ("true", []); ("false", []) ] in
  let option_td =
    make_type_decl ~name:"option" ~params:[ "a" ]
      [ ("None", []); ("Some", [ Types.TyVar 0 ]) ]
  in
  let result_td =
    make_type_decl ~name:"result" ~params:[ "a"; "e" ]
      [ ("Ok", [ Types.TyVar 0 ]); ("Error", [ Types.TyVar 1 ]) ]
  in
  let list_td =
    make_type_decl ~name:"list" ~params:[ "a" ]
      [ ("Nil", []); ("Cons", [ Types.TyVar 0; Types.TyVar 1 ]) ]
  in
  let unit_td = make_type_decl ~name:"unit" [ ("()", []) ] in
  empty_sig_env |> register_type_decl bool_td |> register_type_decl option_td
  |> register_type_decl result_td |> register_type_decl list_td
  |> register_type_decl unit_td
