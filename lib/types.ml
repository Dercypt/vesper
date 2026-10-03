module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type t = TyInt | TyBool | TyString | TyUnit | TyArrow of t * t | TyVar of int
type subst = t IntMap.t
type scheme = Forall of int list * t

let empty_subst : subst = IntMap.empty
let singleton_subst (var : int) (ty : t) : subst = IntMap.singleton var ty

let rec apply (subst : subst) (ty : t) : t =
  match ty with
  | TyInt | TyBool | TyString | TyUnit -> ty
  | TyVar id -> (
      match IntMap.find_opt id subst with
      | Some (TyVar id') when id' = id -> TyVar id
      | Some mapped -> apply (IntMap.remove id subst) mapped
      | None -> TyVar id)
  | TyArrow (t1, t2) -> TyArrow (apply subst t1, apply subst t2)

let rec ftv (ty : t) : IntSet.t =
  match ty with
  | TyInt | TyBool | TyString | TyUnit -> IntSet.empty
  | TyVar id -> IntSet.singleton id
  | TyArrow (t1, t2) -> IntSet.union (ftv t1) (ftv t2)

let ftv_scheme (Forall (vars, body) : scheme) : IntSet.t =
  let body_ftv = ftv body in
  List.fold_left (fun acc v -> IntSet.remove v acc) body_ftv vars

let apply_scheme (subst : subst) (Forall (vars, body) : scheme) : scheme =
  let subst' = List.fold_left (fun s v -> IntMap.remove v s) subst vars in
  Forall (vars, apply subst' body)

let compose_subst (s1 : subst) (s2 : subst) : subst =
  let s2' = IntMap.map (apply s1) s2 in
  IntMap.union (fun _k v2_applied _v1 -> Some v2_applied) s2' s1

let rec equal (t1 : t) (t2 : t) : bool =
  match (t1, t2) with
  | TyInt, TyInt | TyBool, TyBool | TyString, TyString | TyUnit, TyUnit -> true
  | TyVar id1, TyVar id2 -> id1 = id2
  | TyArrow (a1, r1), TyArrow (a2, r2) -> equal a1 a2 && equal r1 r2
  | _ -> false

let normalize_vars (ty : t) : t =
  let counter = ref 0 in
  let mapping = ref IntMap.empty in
  let rec ren ty =
    match ty with
    | TyInt | TyBool | TyString | TyUnit -> ty
    | TyVar id ->
        let new_id =
          match IntMap.find_opt id !mapping with
          | Some nid -> nid
          | None ->
              let nid = !counter in
              incr counter;
              mapping := IntMap.add id nid !mapping;
              nid
        in
        TyVar new_id
    | TyArrow (t1, t2) -> TyArrow (ren t1, ren t2)
  in
  ren ty

let normalize_scheme (Forall (vars, body) : scheme) : scheme =
  let counter = ref 0 in
  let mapping = ref IntMap.empty in
  let rec ren ty =
    match ty with
    | TyInt | TyBool | TyString | TyUnit -> ty
    | TyVar id -> (
        match IntMap.find_opt id !mapping with
        | Some nid -> TyVar nid
        | None ->
            if List.mem id vars then begin
              let nid = !counter in
              incr counter;
              mapping := IntMap.add id nid !mapping;
              TyVar nid
            end
            else TyVar id)
    | TyArrow (t1, t2) -> TyArrow (ren t1, ren t2)
  in
  let body' = ren body in
  let new_vars = List.init !counter (fun i -> i) in
  Forall (new_vars, body')

let scheme_equal (s1 : scheme) (s2 : scheme) : bool =
  let (Forall (v1, b1)) = normalize_scheme s1 in
  let (Forall (v2, b2)) = normalize_scheme s2 in
  v1 = v2 && equal b1 b2

let var_to_string (id : int) : string =
  if id >= 0 && id < 26 then Printf.sprintf "'%c" (Char.chr (Char.code 'a' + id))
  else Printf.sprintf "'t%d" id

let rec to_string (ty : t) : string =
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyString -> "string"
  | TyUnit -> "unit"
  | TyVar id -> var_to_string id
  | TyArrow (t1, t2) ->
      let s1 =
        match t1 with TyArrow _ -> "(" ^ to_string t1 ^ ")" | _ -> to_string t1
      in
      let s2 = to_string t2 in
      s1 ^ " -> " ^ s2

let pp (fmt : Format.formatter) (ty : t) : unit =
  Format.pp_print_string fmt (to_string ty)

let scheme_to_string (Forall (vars, body) : scheme) : string =
  match vars with
  | [] -> to_string body
  | _ ->
      let norm = normalize_scheme (Forall (vars, body)) in
      let (Forall (nvars, nbody)) = norm in
      let var_names = List.map var_to_string nvars in
      Printf.sprintf "forall %s. %s" (String.concat " " var_names) (to_string nbody)

let pp_scheme (fmt : Format.formatter) (s : scheme) : unit =
  Format.pp_print_string fmt (scheme_to_string s)
