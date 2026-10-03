module StringMap = Map.Make (String)

type t = Types.scheme StringMap.t

let empty : t = StringMap.empty
let is_empty (env : t) : bool = StringMap.is_empty env

let extend (name : string) (scheme : Types.scheme) (env : t) : t =
  StringMap.add name scheme env

let extend_type (name : string) (ty : Types.t) (env : t) : t =
  extend name (Types.Forall ([], ty)) env

let lookup (name : string) (env : t) : Types.scheme option = StringMap.find_opt name env
let mem (name : string) (env : t) : bool = StringMap.mem name env
let remove (name : string) (env : t) : t = StringMap.remove name env

let apply_subst (subst : Types.subst) (env : t) : t =
  StringMap.map (Types.apply_scheme subst) env

let ftv (env : t) : Types.IntSet.t =
  StringMap.fold
    (fun _name scheme acc -> Types.IntSet.union (Types.ftv_scheme scheme) acc)
    env Types.IntSet.empty

let max_var (env : t) : int =
  let vars = ftv env in
  if Types.IntSet.is_empty vars then -1 else Types.IntSet.max_elt vars

let generalize (env : t) (ty : Types.t) : Types.scheme =
  let env_ftv = ftv env in
  let ty_ftv = Types.ftv ty in
  let gen_vars = Types.IntSet.diff ty_ftv env_ftv in
  Types.Forall (Types.IntSet.elements gen_vars, ty)

let instantiate (fresh_fn : unit -> Types.t) (Forall (vars, body) : Types.scheme) :
    Types.t =
  match vars with
  | [] -> body
  | _ ->
      let subst =
        List.fold_left
          (fun s v -> Types.IntMap.add v (fresh_fn ()) s)
          Types.empty_subst vars
      in
      Types.apply subst body

let to_list (env : t) : (string * Types.scheme) list = StringMap.bindings env

let of_list (bindings : (string * Types.scheme) list) : t =
  List.fold_left (fun acc (name, scheme) -> StringMap.add name scheme acc) empty bindings

let to_string (env : t) : string =
  let bindings =
    to_list env
    |> List.map (fun (name, scheme) ->
        Printf.sprintf "%s : %s" name (Types.scheme_to_string scheme))
  in
  "{" ^ String.concat ", " bindings ^ "}"

let pp (fmt : Format.formatter) (env : t) : unit =
  Format.pp_print_string fmt (to_string env)
