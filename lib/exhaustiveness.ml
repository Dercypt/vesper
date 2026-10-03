type matrix = Pattern.pattern list list

type check_result = {
  is_exhaustive : bool;
  missing_witness : Pattern.pattern option;
  redundant_indices : int list;
  diagnostics : Diagnostic.t list;
}

type head_ctor =
  | Ctor of string * int
  | CtorBool of bool
  | CtorInt of int
  | CtorString of string
  | CtorTuple of int

let ctor_equal (a : head_ctor) (b : head_ctor) : bool =
  match (a, b) with
  | Ctor (n1, a1), Ctor (n2, a2) -> String.equal n1 n2 && a1 = a2
  | CtorBool b1, CtorBool b2 -> b1 = b2
  | CtorInt i1, CtorInt i2 -> i1 = i2
  | CtorString s1, CtorString s2 -> String.equal s1 s2
  | CtorTuple t1, CtorTuple t2 -> t1 = t2
  | _ -> false

let head_ctor_of_pattern (sig_env : Pattern.signature_env) (p : Pattern.pattern) :
    head_ctor option =
  match p.desc with
  | PConstructor (name, args) -> Some (Ctor (name, List.length args))
  | PLit (Bool b) -> Some (CtorBool b)
  | PLit (Int n) -> Some (CtorInt n)
  | PLit (String s) -> Some (CtorString s)
  | PTuple elems -> Some (CtorTuple (List.length elems))
  | PRecord fields -> (
      match fields with
      | (first_f, _) :: _ -> (
          let matching_record =
            match Pattern.lookup_record first_f sig_env with
            | Some rd -> Some rd
            | None -> None
          in
          match matching_record with
          | Some rd -> Some (Ctor (rd.name, List.length rd.fields))
          | None -> Some (Ctor ("{record}", List.length fields)))
      | [] -> None)
  | _ -> None

let rec expand_row (row : Pattern.pattern list) : Pattern.pattern list list =
  match row with
  | [] -> [ [] ]
  | p :: rest -> (
      let rest_expanded = expand_row rest in
      match p.desc with
      | POr (p1, p2) ->
          let rows1 = expand_row (p1 :: rest) in
          let rows2 = expand_row (p2 :: rest) in
          rows1 @ rows2
      | PAlias (inner, _) -> List.map (fun r -> inner :: r) rest_expanded
      | _ -> List.map (fun r -> p :: r) rest_expanded)

let expand_matrix (m : matrix) : matrix = List.concat_map expand_row m

let specialize_row (target : head_ctor) (row : Pattern.pattern list) :
    Pattern.pattern list option =
  match row with
  | [] -> None
  | p :: rest -> (
      match (target, p.desc) with
      | Ctor (c_name, arity), PConstructor (name, args) ->
          if String.equal c_name name && arity = List.length args then Some (args @ rest)
          else None
      | CtorBool b_target, PLit (Bool b) -> if b_target = b then Some rest else None
      | CtorInt i_target, PLit (Int i) -> if i_target = i then Some rest else None
      | CtorString s_target, PLit (String s) ->
          if String.equal s_target s then Some rest else None
      | CtorTuple t_target, PTuple elems ->
          if t_target = List.length elems then Some (elems @ rest) else None
      | Ctor (c_name, _), PRecord fields ->
          let sorted_fields =
            List.sort (fun (a, _) (b, _) -> String.compare a b) fields
          in
          let args = List.map snd sorted_fields in
          if String.equal c_name "{record}" || List.length args = 0 then Some (args @ rest)
          else None
      | Ctor (_, arity), (PWildcard | PVar _) ->
          let wilds = List.init arity (fun _ -> Pattern.wildcard ~span:p.span) in
          Some (wilds @ rest)
      | (CtorBool _ | CtorInt _ | CtorString _), (PWildcard | PVar _) -> Some rest
      | CtorTuple arity, (PWildcard | PVar _) ->
          let wilds = List.init arity (fun _ -> Pattern.wildcard ~span:p.span) in
          Some (wilds @ rest)
      | _ -> None)

let specialize_matrix (target : head_ctor) (m : matrix) : matrix =
  List.filter_map (specialize_row target) (expand_matrix m)

let default_row (row : Pattern.pattern list) : Pattern.pattern list option =
  match row with
  | [] -> None
  | p :: rest -> ( match p.desc with PWildcard | PVar _ -> Some rest | _ -> None)

let default_matrix (m : matrix) : matrix = List.filter_map default_row (expand_matrix m)

let collect_head_ctors (sig_env : Pattern.signature_env) (m : matrix) : head_ctor list =
  let ctors =
    List.filter_map
      (function p :: _ -> head_ctor_of_pattern sig_env p | [] -> None)
      (expand_matrix m)
  in
  List.fold_left
    (fun acc c -> if List.exists (ctor_equal c) acc then acc else c :: acc)
    [] ctors

type signature_status =
  | Complete of head_ctor list
  | Incomplete of head_ctor list
  | InfiniteOrUnknown

let check_signature (sig_env : Pattern.signature_env) (ctors : head_ctor list)
    (expected_type : string option) : signature_status =
  match ctors with
  | [] -> (
      match expected_type with
      | Some "bool" -> Incomplete [ CtorBool true; CtorBool false ]
      | Some tname -> (
          match Pattern.constructors_of_type tname sig_env with
          | Some full_list ->
              Incomplete (List.map (fun (name, arity) -> Ctor (name, arity)) full_list)
          | None -> InfiniteOrUnknown)
      | None -> InfiniteOrUnknown)
  | CtorBool _ :: _ ->
      let has_true = List.exists (function CtorBool true -> true | _ -> false) ctors in
      let has_false =
        List.exists (function CtorBool false -> true | _ -> false) ctors
      in
      if has_true && has_false then Complete [ CtorBool true; CtorBool false ]
      else if not has_true then Incomplete [ CtorBool true ]
      else Incomplete [ CtorBool false ]
  | CtorInt _ :: _ | CtorString _ :: _ -> InfiniteOrUnknown
  | CtorTuple n :: _ -> Complete [ CtorTuple n ]
  | Ctor ("{record}", n) :: _ -> Complete [ Ctor ("{record}", n) ]
  | Ctor (first_name, _) :: _ -> (
      match Pattern.lookup_record first_name sig_env with
      | Some rd -> Complete [ Ctor (rd.name, List.length rd.fields) ]
      | None -> (
          let tname_opt =
            match expected_type with
            | Some t -> Some t
            | None -> (
                match Pattern.lookup_constructor first_name sig_env with
                | Some info -> Some info.type_name
                | None -> None)
          in
          match tname_opt with
          | Some tname -> (
              match Pattern.constructors_of_type tname sig_env with
              | Some all_variants ->
                  let all_ctors =
                    List.map (fun (name, arity) -> Ctor (name, arity)) all_variants
                  in
                  let missing =
                    List.filter
                      (fun target -> not (List.exists (ctor_equal target) ctors))
                      all_ctors
                  in
                  if missing = [] then Complete all_ctors else Incomplete missing
              | None -> InfiniteOrUnknown)
          | None -> InfiniteOrUnknown))

let rec useful (sig_env : Pattern.signature_env) (m : matrix) (vec : Pattern.pattern list)
    (expected_type : string option) : bool =
  match vec with
  | [] -> m = []
  | q :: rest_vec -> (
      match q.desc with
      | POr (p1, p2) ->
          useful sig_env m (p1 :: rest_vec) expected_type
          || useful sig_env m (p2 :: rest_vec) expected_type
      | PAlias (inner, _) -> useful sig_env m (inner :: rest_vec) expected_type
      | PConstructor (name, args) ->
          let target = Ctor (name, List.length args) in
          let specialized = specialize_matrix target m in
          useful sig_env specialized (args @ rest_vec) None
      | PLit (Bool b) ->
          let target = CtorBool b in
          let specialized = specialize_matrix target m in
          useful sig_env specialized rest_vec None
      | PLit (Int n) ->
          let target = CtorInt n in
          let specialized = specialize_matrix target m in
          useful sig_env specialized rest_vec None
      | PLit (String s) ->
          let target = CtorString s in
          let specialized = specialize_matrix target m in
          useful sig_env specialized rest_vec None
      | PTuple elems ->
          let target = CtorTuple (List.length elems) in
          let specialized = specialize_matrix target m in
          useful sig_env specialized (elems @ rest_vec) None
      | PRecord fields ->
          let target = Ctor ("{record}", List.length fields) in
          let specialized = specialize_matrix target m in
          let sorted_fields =
            List.sort (fun (a, _) (b, _) -> String.compare a b) fields
          in
          let args = List.map snd sorted_fields in
          useful sig_env specialized (args @ rest_vec) None
      | PWildcard | PVar _ -> (
          let head_ctors = collect_head_ctors sig_env m in
          let status = check_signature sig_env head_ctors expected_type in
          match status with
          | Complete all_ctors ->
              List.exists
                (fun ctor ->
                  let specialized = specialize_matrix ctor m in
                  let arity =
                    match ctor with
                    | Ctor (_, a) -> a
                    | CtorTuple a -> a
                    | CtorBool _ | CtorInt _ | CtorString _ -> 0
                  in
                  let wilds = List.init arity (fun _ -> Pattern.wildcard ~span:q.span) in
                  useful sig_env specialized (wilds @ rest_vec) None)
                all_ctors
          | Incomplete _ | InfiniteOrUnknown ->
              let def = default_matrix m in
              useful sig_env def rest_vec None))

let is_useful ~sig_env ~matrix ~vector = useful sig_env matrix vector None

let rec build_witness (sig_env : Pattern.signature_env) (span : Ast.span) (m : matrix)
    (vec : Pattern.pattern list) (expected_type : string option) :
    Pattern.pattern list option =
  match vec with
  | [] -> if m = [] then Some [] else None
  | _q :: rest_vec -> (
      let head_ctors = collect_head_ctors sig_env m in
      let status = check_signature sig_env head_ctors expected_type in
      match status with
      | Complete all_ctors ->
          let rec try_ctors = function
            | [] -> None
            | ctor :: rest_ctors -> (
                let specialized = specialize_matrix ctor m in
                let arity =
                  match ctor with
                  | Ctor (_, a) -> a
                  | CtorTuple a -> a
                  | CtorBool _ | CtorInt _ | CtorString _ -> 0
                in
                let wilds = List.init arity (fun _ -> Pattern.wildcard ~span) in
                match build_witness sig_env span specialized (wilds @ rest_vec) None with
                | Some sub_witness ->
                    let ctor_args, remaining_witness =
                      let rec take_n n acc = function
                        | l when n <= 0 -> (List.rev acc, l)
                        | h :: t -> take_n (n - 1) (h :: acc) t
                        | [] -> (List.rev acc, [])
                      in
                      take_n arity [] sub_witness
                    in
                    let pat_desc =
                      match ctor with
                      | Ctor ("{record}", _) ->
                          Pattern.PRecord
                            (List.mapi (fun i a -> (Printf.sprintf "f%d" i, a)) ctor_args)
                      | Ctor (name, _) -> Pattern.PConstructor (name, ctor_args)
                      | CtorBool b -> Pattern.PLit (Ast.Bool b)
                      | CtorInt i -> Pattern.PLit (Ast.Int i)
                      | CtorString s -> Pattern.PLit (Ast.String s)
                      | CtorTuple _ -> Pattern.PTuple ctor_args
                    in
                    Some ({ Pattern.span; desc = pat_desc } :: remaining_witness)
                | None -> try_ctors rest_ctors)
          in
          try_ctors all_ctors
      | Incomplete missing_ctors -> (
          match missing_ctors with
          | missing :: _ -> (
              let def = default_matrix m in
              match build_witness sig_env span def rest_vec None with
              | Some rest_w ->
                  let arity =
                    match missing with
                    | Ctor (_, a) -> a
                    | CtorTuple a -> a
                    | CtorBool _ | CtorInt _ | CtorString _ -> 0
                  in
                  let wilds = List.init arity (fun _ -> Pattern.wildcard ~span) in
                  let pat_desc =
                    match missing with
                    | Ctor (name, _) -> Pattern.PConstructor (name, wilds)
                    | CtorBool b -> Pattern.PLit (Ast.Bool b)
                    | CtorInt i -> Pattern.PLit (Ast.Int i)
                    | CtorString s -> Pattern.PLit (Ast.String s)
                    | CtorTuple _ -> Pattern.PTuple wilds
                  in
                  Some ({ Pattern.span; desc = pat_desc } :: rest_w)
              | None -> None)
          | [] -> None)
      | InfiniteOrUnknown -> (
          let def = default_matrix m in
          match build_witness sig_env span def rest_vec None with
          | Some rest_w ->
              let witness_head =
                if List.exists (function CtorInt _ -> true | _ -> false) head_ctors then
                  let used_ints =
                    List.filter_map
                      (function CtorInt i -> Some i | _ -> None)
                      head_ctors
                  in
                  let fresh_int =
                    match used_ints with [] -> 0 | l -> List.fold_left max 0 l + 1
                  in
                  Pattern.int ~span fresh_int
                else if
                  List.exists (function CtorString _ -> true | _ -> false) head_ctors
                then Pattern.string ~span "_"
                else Pattern.wildcard ~span
              in
              Some (witness_head :: rest_w)
          | None -> None))

let find_witness ~sig_env ~span ~matrix ?expected_type () =
  let wild = [ Pattern.wildcard ~span ] in
  match build_witness sig_env span matrix wild expected_type with
  | Some (w :: _) -> Some w
  | _ -> None

let check_patterns ~sig_env ~span ?expected_type (patterns : Pattern.pattern list) :
    check_result =
  let rec check_redundancy idx (cur_matrix : matrix) pats =
    match pats with
    | [] -> ([], cur_matrix)
    | p :: rest ->
        let expanded = expand_row [ p ] in
        let is_any_useful =
          List.exists (fun row -> useful sig_env cur_matrix row expected_type) expanded
        in
        if not is_any_useful then
          let redundant_indices, final_matrix =
            check_redundancy (idx + 1) cur_matrix rest
          in
          (idx :: redundant_indices, final_matrix)
        else
          let new_matrix = cur_matrix @ expanded in
          let redundant_indices, final_matrix =
            check_redundancy (idx + 1) new_matrix rest
          in
          (redundant_indices, final_matrix)
  in
  let redundant_indices, full_matrix = check_redundancy 0 [] patterns in
  let missing_witness =
    find_witness ~sig_env ~span ~matrix:full_matrix ?expected_type ()
  in
  let is_exhaustive = missing_witness = None in
  let redundant_diagnostics =
    List.map
      (fun i ->
        let p = List.nth patterns i in
        Diagnostic.warning ~code:Error_code.E3002_redundant_pattern ~span:p.span
          ~hint:
            "Consider removing this unreachable branch or reordering earlier patterns."
          "This pattern clause is redundant and will never be matched.")
      redundant_indices
  in
  let exhaustiveness_diagnostics =
    match missing_witness with
    | Some w ->
        let witness_str = Pattern.to_string w in
        [
          Diagnostic.error ~code:Error_code.E3001_non_exhaustive_match ~span
            ~hint:
              (Printf.sprintf "Add a clause covering '%s' or a wildcard pattern '_'."
                 witness_str)
            (Printf.sprintf
               "Pattern matching is non-exhaustive. Example case not matched: %s"
               witness_str);
        ]
    | None -> []
  in
  {
    is_exhaustive;
    missing_witness;
    redundant_indices;
    diagnostics = redundant_diagnostics @ exhaustiveness_diagnostics;
  }

let check_clauses ~sig_env ~span ?expected_type (clauses : Pattern.clause list) :
    check_result =
  let patterns = List.map (fun (c : Pattern.clause) -> c.pattern) clauses in
  check_patterns ~sig_env ~span ?expected_type patterns
