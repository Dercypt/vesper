let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

type path = Here | Field of string * path | Arg of int * path | TupleElem of int * path

let rec path_to_string = function
  | Here -> "$"
  | Field (f, p) -> Printf.sprintf "%s.%s" (path_to_string p) f
  | Arg (i, p) -> Printf.sprintf "%s[%d]" (path_to_string p) i
  | TupleElem (i, p) -> Printf.sprintf "%s.(%d)" (path_to_string p) i

type action = {
  action_id : int;
  span : Ast.span;
  bindings : (string * path) list;
  guard : Core_ir.expr option;
  body : Core_ir.expr;
}

type decision_tree =
  | Leaf of action
  | SwitchLit of {
      target : path;
      span : Ast.span;
      cases : (Ast.literal * decision_tree) list;
      default : decision_tree option;
    }
  | SwitchConstructor of {
      target : path;
      span : Ast.span;
      cases : (string * decision_tree) list;
      default : decision_tree option;
    }
  | SwitchBool of {
      target : path;
      span : Ast.span;
      true_branch : decision_tree;
      false_branch : decision_tree;
    }
  | Guard of {
      bindings : (string * path) list;
      condition : Core_ir.expr;
      then_tree : decision_tree;
      else_tree : decision_tree;
    }
  | Failure of { span : Ast.span }

let rec equal_path (p1 : path) (p2 : path) : bool =
  match (p1, p2) with
  | Here, Here -> true
  | Field (f1, r1), Field (f2, r2) -> String.equal f1 f2 && equal_path r1 r2
  | Arg (i1, r1), Arg (i2, r2) -> i1 = i2 && equal_path r1 r2
  | TupleElem (i1, r1), TupleElem (i2, r2) -> i1 = i2 && equal_path r1 r2
  | _ -> false

let equal_action (a1 : action) (a2 : action) : bool =
  a1.action_id = a2.action_id
  && List.equal
       (fun (k1, p1) (k2, p2) -> String.equal k1 k2 && equal_path p1 p2)
       a1.bindings a2.bindings
  && Core_ir.expr_equal a1.body a2.body

let rec equal_tree (t1 : decision_tree) (t2 : decision_tree) : bool =
  match (t1, t2) with
  | Leaf a1, Leaf a2 -> equal_action a1 a2
  | ( SwitchLit { target = p1; cases = c1; default = d1; _ },
      SwitchLit { target = p2; cases = c2; default = d2; _ } ) ->
      equal_path p1 p2
      && List.equal (fun (l1, tr1) (l2, tr2) -> l1 = l2 && equal_tree tr1 tr2) c1 c2
      && Option.equal equal_tree d1 d2
  | ( SwitchConstructor { target = p1; cases = c1; default = d1; _ },
      SwitchConstructor { target = p2; cases = c2; default = d2; _ } ) ->
      equal_path p1 p2
      && List.equal
           (fun (k1, tr1) (k2, tr2) -> String.equal k1 k2 && equal_tree tr1 tr2)
           c1 c2
      && Option.equal equal_tree d1 d2
  | ( SwitchBool { target = p1; true_branch = tb1; false_branch = fb1; _ },
      SwitchBool { target = p2; true_branch = tb2; false_branch = fb2; _ } ) ->
      equal_path p1 p2 && equal_tree tb1 tb2 && equal_tree fb1 fb2
  | ( Guard { bindings = b1; condition = c1; then_tree = tt1; else_tree = et1 },
      Guard { bindings = b2; condition = c2; then_tree = tt2; else_tree = et2 } ) ->
      List.equal (fun (k1, p1) (k2, p2) -> String.equal k1 k2 && equal_path p1 p2) b1 b2
      && Core_ir.expr_equal c1 c2 && equal_tree tt1 tt2 && equal_tree et1 et2
  | Failure _, Failure _ -> true
  | _ -> false

let rec pp fmt = function
  | Leaf a ->
      Format.fprintf fmt "Leaf(action=%d, binds=[%a])" a.action_id
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (v, p) -> Format.fprintf fmt "%s->%s" v (path_to_string p)))
        a.bindings
  | SwitchLit { target; cases; default; _ } ->
      Format.fprintf fmt "SwitchLit(%s, cases=[%a], default=%s)" (path_to_string target)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (l, tr) ->
             let lit_str =
               match l with
               | Ast.Int n -> string_of_int n
               | Ast.Bool b -> string_of_bool b
               | Ast.String s -> Printf.sprintf "%S" s
             in
             Format.fprintf fmt "%s => %a" lit_str pp tr))
        cases
        (match default with Some d -> to_string d | None -> "none")
  | SwitchConstructor { target; cases; default; _ } ->
      Format.fprintf fmt "SwitchCtor(%s, cases=[%a], default=%s)" (path_to_string target)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (c, tr) -> Format.fprintf fmt "%s => %a" c pp tr))
        cases
        (match default with Some d -> to_string d | None -> "none")
  | SwitchBool { target; true_branch; false_branch; _ } ->
      Format.fprintf fmt "SwitchBool(%s, true => %a, false => %a)" (path_to_string target)
        pp true_branch pp false_branch
  | Guard { condition = _; then_tree; else_tree; _ } ->
      Format.fprintf fmt "Guard(then => %a, else => %a)" pp then_tree pp else_tree
  | Failure _ -> Format.fprintf fmt "Failure"

and to_string t = Format.asprintf "%a" pp t

type compile_row = {
  pats : (path * Pattern.pattern) list;
  bindings : (string * path) list;
  action_id : int;
  span : Ast.span;
  guard : Core_ir.expr option;
  body : Core_ir.expr;
}

let rec decompose_matrix (sig_env : Pattern.signature_env) (span : Ast.span)
    (rows : compile_row list) : (decision_tree, Ast.error) result =
  match rows with
  | [] -> Ok (Failure { span })
  | first_row :: _ -> (
      match first_row.pats with
      | [] -> (
          let act =
            {
              action_id = first_row.action_id;
              span = first_row.span;
              bindings = first_row.bindings;
              guard = first_row.guard;
              body = first_row.body;
            }
          in
          match first_row.guard with
          | Some g ->
              let* else_tree = decompose_matrix sig_env span (List.tl rows) in
              Ok
                (Guard
                   {
                     bindings = first_row.bindings;
                     condition = g;
                     then_tree = Leaf act;
                     else_tree;
                   })
          | None -> Ok (Leaf act))
      | (curr_path, first_pat) :: _ -> (
          match first_pat.desc with
          | Pattern.PWildcard ->
              let next_rows =
                List.map
                  (fun r ->
                    match r.pats with
                    | (_, p) :: rest_pats -> (
                        match p.desc with
                        | Pattern.PVar name ->
                            {
                              r with
                              pats = rest_pats;
                              bindings = (name, curr_path) :: r.bindings;
                            }
                        | _ -> { r with pats = rest_pats })
                    | [] -> r)
                  rows
              in
              decompose_matrix sig_env span next_rows
          | Pattern.PVar _ ->
              let next_rows =
                List.map
                  (fun r ->
                    match r.pats with
                    | (_, p) :: rest_pats -> (
                        match p.desc with
                        | Pattern.PVar v ->
                            {
                              r with
                              pats = rest_pats;
                              bindings = (v, curr_path) :: r.bindings;
                            }
                        | _ -> { r with pats = rest_pats })
                    | [] -> r)
                  rows
              in
              decompose_matrix sig_env span next_rows
          | Pattern.PAlias (inner, name) ->
              let updated_first =
                {
                  first_row with
                  pats = (curr_path, inner) :: List.tl first_row.pats;
                  bindings = (name, curr_path) :: first_row.bindings;
                }
              in
              decompose_matrix sig_env span (updated_first :: List.tl rows)
          | Pattern.POr (p1, p2) ->
              let r1 =
                { first_row with pats = (curr_path, p1) :: List.tl first_row.pats }
              in
              let r2 =
                { first_row with pats = (curr_path, p2) :: List.tl first_row.pats }
              in
              decompose_matrix sig_env span (r1 :: r2 :: List.tl rows)
          | Pattern.PTuple elems ->
              let decomposed_rows =
                List.map
                  (fun r ->
                    match r.pats with
                    | (p, { desc = Pattern.PTuple row_elems; _ }) :: rest_pats ->
                        let new_sub_pats =
                          List.mapi (fun i elem -> (TupleElem (i, p), elem)) row_elems
                        in
                        { r with pats = new_sub_pats @ rest_pats }
                    | (p, { desc = Pattern.PWildcard | Pattern.PVar _; _ }) :: rest_pats
                      ->
                        let new_sub_pats =
                          List.mapi
                            (fun i _ ->
                              (TupleElem (i, p), Pattern.wildcard ~span:first_pat.span))
                            elems
                        in
                        { r with pats = new_sub_pats @ rest_pats }
                    | _ -> r)
                  rows
              in
              decompose_matrix sig_env span decomposed_rows
          | Pattern.PRecord fields ->
              let decomposed_rows =
                List.map
                  (fun r ->
                    match r.pats with
                    | (p, { desc = Pattern.PRecord row_fields; _ }) :: rest_pats ->
                        let new_sub_pats =
                          List.map
                            (fun (fname, fpat) -> (Field (fname, p), fpat))
                            row_fields
                        in
                        { r with pats = new_sub_pats @ rest_pats }
                    | (p, { desc = Pattern.PWildcard | Pattern.PVar _; _ }) :: rest_pats
                      ->
                        let new_sub_pats =
                          List.map
                            (fun (fname, _) ->
                              (Field (fname, p), Pattern.wildcard ~span:first_pat.span))
                            fields
                        in
                        { r with pats = new_sub_pats @ rest_pats }
                    | _ -> r)
                  rows
              in
              decompose_matrix sig_env span decomposed_rows
          | Pattern.PLit (Bool _) ->
              let specialize_bool b =
                List.filter_map
                  (fun r ->
                    match r.pats with
                    | (_, { desc = Pattern.PLit (Bool row_b); _ }) :: rest_pats ->
                        if b = row_b then Some { r with pats = rest_pats } else None
                    | (_, { desc = Pattern.PWildcard; _ }) :: rest_pats ->
                        Some { r with pats = rest_pats }
                    | (_, { desc = Pattern.PVar name; _ }) :: rest_pats ->
                        Some
                          {
                            r with
                            pats = rest_pats;
                            bindings = (name, curr_path) :: r.bindings;
                          }
                    | _ -> None)
                  rows
              in
              let* true_branch = decompose_matrix sig_env span (specialize_bool true) in
              let* false_branch = decompose_matrix sig_env span (specialize_bool false) in
              Ok (SwitchBool { target = curr_path; span; true_branch; false_branch })
          | Pattern.PLit _ ->
              let distinct_lits =
                List.fold_left
                  (fun acc r ->
                    match r.pats with
                    | (_, { desc = Pattern.PLit l; _ }) :: _ ->
                        if List.mem l acc then acc else l :: acc
                    | _ -> acc)
                  [] rows
              in
              let* cases =
                List.fold_left
                  (fun acc lit ->
                    let* prev = acc in
                    let spec_rows =
                      List.filter_map
                        (fun r ->
                          match r.pats with
                          | (_, { desc = Pattern.PLit row_l; _ }) :: rest_pats ->
                              if lit = row_l then Some { r with pats = rest_pats }
                              else None
                          | (_, { desc = Pattern.PWildcard; _ }) :: rest_pats ->
                              Some { r with pats = rest_pats }
                          | (_, { desc = Pattern.PVar name; _ }) :: rest_pats ->
                              Some
                                {
                                  r with
                                  pats = rest_pats;
                                  bindings = (name, curr_path) :: r.bindings;
                                }
                          | _ -> None)
                        rows
                    in
                    let* subtree = decompose_matrix sig_env span spec_rows in
                    Ok ((lit, subtree) :: prev))
                  (Ok []) distinct_lits
              in
              let def_rows =
                List.filter_map
                  (fun r ->
                    match r.pats with
                    | (_, { desc = Pattern.PWildcard; _ }) :: rest_pats ->
                        Some { r with pats = rest_pats }
                    | (_, { desc = Pattern.PVar name; _ }) :: rest_pats ->
                        Some
                          {
                            r with
                            pats = rest_pats;
                            bindings = (name, curr_path) :: r.bindings;
                          }
                    | _ -> None)
                  rows
              in
              let* default =
                if def_rows = [] then Ok None
                else
                  let* tree = decompose_matrix sig_env span def_rows in
                  Ok (Some tree)
              in
              Ok (SwitchLit { target = curr_path; span; cases = List.rev cases; default })
          | Pattern.PConstructor (cname, _) ->
              let distinct_ctors =
                List.fold_left
                  (fun acc r ->
                    match r.pats with
                    | (_, { desc = Pattern.PConstructor (c, args); _ }) :: _ ->
                        if List.mem_assoc c acc then acc else (c, List.length args) :: acc
                    | _ -> acc)
                  [] rows
              in
              let* cases =
                List.fold_left
                  (fun acc (c, arity) ->
                    let* prev = acc in
                    let spec_rows =
                      List.filter_map
                        (fun r ->
                          match r.pats with
                          | (p, { desc = Pattern.PConstructor (rc, rargs); _ })
                            :: rest_pats ->
                              if String.equal c rc then
                                let sub_pats =
                                  List.mapi (fun i a -> (Arg (i, p), a)) rargs
                                in
                                Some { r with pats = sub_pats @ rest_pats }
                              else None
                          | (p, { desc = Pattern.PWildcard; _ }) :: rest_pats ->
                              let sub_pats =
                                List.init arity (fun i ->
                                    (Arg (i, p), Pattern.wildcard ~span:first_pat.span))
                              in
                              Some { r with pats = sub_pats @ rest_pats }
                          | (p, { desc = Pattern.PVar name; _ }) :: rest_pats ->
                              let sub_pats =
                                List.init arity (fun i ->
                                    (Arg (i, p), Pattern.wildcard ~span:first_pat.span))
                              in
                              Some
                                {
                                  r with
                                  pats = sub_pats @ rest_pats;
                                  bindings = (name, curr_path) :: r.bindings;
                                }
                          | _ -> None)
                        rows
                    in
                    let* subtree = decompose_matrix sig_env span spec_rows in
                    Ok ((c, subtree) :: prev))
                  (Ok []) distinct_ctors
              in
              let tname_opt =
                match Pattern.lookup_constructor cname sig_env with
                | Some info -> Some info.type_name
                | None -> None
              in
              let is_complete =
                match tname_opt with
                | Some tname -> (
                    match Pattern.constructors_of_type tname sig_env with
                    | Some all_variants ->
                        List.for_all
                          (fun (v, _) -> List.mem_assoc v distinct_ctors)
                          all_variants
                    | None -> false)
                | None -> false
              in
              let def_rows =
                List.filter_map
                  (fun r ->
                    match r.pats with
                    | (_, { desc = Pattern.PWildcard; _ }) :: rest_pats ->
                        Some { r with pats = rest_pats }
                    | (_, { desc = Pattern.PVar name; _ }) :: rest_pats ->
                        Some
                          {
                            r with
                            pats = rest_pats;
                            bindings = (name, curr_path) :: r.bindings;
                          }
                    | _ -> None)
                  rows
              in
              let* default =
                if is_complete || def_rows = [] then Ok None
                else
                  let* tree = decompose_matrix sig_env span def_rows in
                  Ok (Some tree)
              in
              Ok
                (SwitchConstructor
                   { target = curr_path; span; cases = List.rev cases; default })))

let compile ~sig_env ~span ~target_expr:_ (clauses : Pattern.clause list) :
    (decision_tree, Ast.error) result =
  let initial_rows =
    List.mapi
      (fun idx (c : Pattern.clause) ->
        {
          pats = [ (Here, c.pattern) ];
          bindings = [];
          action_id = idx;
          span = c.span;
          guard = c.guard;
          body = c.body;
        })
      clauses
  in
  decompose_matrix sig_env span initial_rows

let rec resolve_path (root : Value.t) (p : path) : (Value.t, Ast.error) result =
  match p with
  | Here -> Ok root
  | Field (f, rest) -> (
      let* parent = resolve_path root rest in
      match parent with
      | Value.VRecord fields -> (
          match List.assoc_opt f fields with
          | Some v -> Ok v
          | None ->
              Error
                Ast.
                  {
                    span = Ast.dummy_span;
                    message = Printf.sprintf "Record field '%s' not found" f;
                  })
      | other ->
          Error
            Ast.
              {
                span = Ast.dummy_span;
                message =
                  Printf.sprintf "Expected record value at field '%s', got %s" f
                    (Value.type_of other);
              })
  | Arg (i, rest) -> (
      let* parent = resolve_path root rest in
      match parent with
      | Value.VConstruct (_, args) ->
          if i >= 0 && i < List.length args then Ok (List.nth args i)
          else
            Error
              Ast.
                {
                  span = Ast.dummy_span;
                  message = Printf.sprintf "Constructor argument index %d out of bounds" i;
                }
      | other ->
          Error
            Ast.
              {
                span = Ast.dummy_span;
                message =
                  Printf.sprintf "Expected constructor value at arg %d, got %s" i
                    (Value.type_of other);
              })
  | TupleElem (i, rest) -> (
      let* parent = resolve_path root rest in
      match parent with
      | Value.VConstruct ("(tuple)", args) ->
          if i >= 0 && i < List.length args then Ok (List.nth args i)
          else
            Error
              Ast.
                {
                  span = Ast.dummy_span;
                  message = Printf.sprintf "Tuple element index %d out of bounds" i;
                }
      | other ->
          Error
            Ast.
              {
                span = Ast.dummy_span;
                message =
                  Printf.sprintf "Expected tuple value at element %d, got %s" i
                    (Value.type_of other);
              })

let rec eval_tree ?(fuel = 10000) (env : Env.t) (root_val : Value.t)
    (tree : decision_tree) : (Value.t, Ast.error) result =
  if fuel <= 0 then
    Error
      Ast.
        {
          span = Ast.dummy_span;
          message = "Execution fuel exhausted during pattern matching evaluation";
        }
  else
    match tree with
    | Leaf act ->
        let* bound_env =
          List.fold_left
            (fun acc (var_name, p) ->
              let* cur_env = acc in
              let* v = resolve_path root_val p in
              Ok (Env.extend var_name v cur_env))
            (Ok env) act.bindings
        in
        Eval.eval_expr ~fuel:(fuel - 1) bound_env act.body
    | SwitchBool { target; true_branch; false_branch; _ } -> (
        let* tv = resolve_path root_val target in
        match tv with
        | Value.VBool true -> eval_tree ~fuel:(fuel - 1) env root_val true_branch
        | Value.VBool false -> eval_tree ~fuel:(fuel - 1) env root_val false_branch
        | other ->
            Error
              Ast.
                {
                  span = Ast.dummy_span;
                  message =
                    Printf.sprintf "Expected bool in match switch, got %s"
                      (Value.type_of other);
                })
    | SwitchLit { target; span; cases; default } ->
        let* tv = resolve_path root_val target in
        let rec find_case = function
          | [] -> (
              match default with
              | Some def_tree -> eval_tree ~fuel:(fuel - 1) env root_val def_tree
              | None ->
                  Error Ast.{ span; message = "No matching literal pattern in switch" })
          | (lit, subtree) :: rest_cases ->
              let lit_matches =
                match (lit, tv) with
                | Ast.Int n, Value.VInt vn -> n = vn
                | Ast.Bool b, Value.VBool vb -> b = vb
                | Ast.String s, Value.VString vs -> String.equal s vs
                | _ -> false
              in
              if lit_matches then eval_tree ~fuel:(fuel - 1) env root_val subtree
              else find_case rest_cases
        in
        find_case cases
    | SwitchConstructor { target; span; cases; default } -> (
        let* tv = resolve_path root_val target in
        match tv with
        | Value.VConstruct (tag, _) -> (
            match List.assoc_opt tag cases with
            | Some subtree -> eval_tree ~fuel:(fuel - 1) env root_val subtree
            | None -> (
                match default with
                | Some def_tree -> eval_tree ~fuel:(fuel - 1) env root_val def_tree
                | None ->
                    Error
                      Ast.
                        {
                          span;
                          message =
                            Printf.sprintf "Unhandled constructor '%s' in pattern match"
                              tag;
                        }))
        | other ->
            Error
              Ast.
                {
                  span;
                  message =
                    Printf.sprintf "Expected constructor in switch, got %s"
                      (Value.type_of other);
                })
    | Guard { bindings; condition; then_tree; else_tree } -> (
        let* bound_env =
          List.fold_left
            (fun acc (var_name, p) ->
              let* cur_env = acc in
              let* v = resolve_path root_val p in
              Ok (Env.extend var_name v cur_env))
            (Ok env) bindings
        in
        let* gv = Eval.eval_expr ~fuel:(fuel - 1) bound_env condition in
        match gv with
        | Value.VBool true -> eval_tree ~fuel:(fuel - 1) bound_env root_val then_tree
        | Value.VBool false -> eval_tree ~fuel:(fuel - 1) env root_val else_tree
        | other ->
            Error
              Ast.
                {
                  span = condition.span;
                  message =
                    Printf.sprintf "Match guard must evaluate to bool, got %s"
                      (Value.type_of other);
                })
    | Failure { span } ->
        Error
          Ast.{ span; message = "Pattern match failure: no clause matched runtime value" }

let eval_match ?(fuel = 10000) ~sig_env ~span (env : Env.t) (root_val : Value.t)
    (clauses : Pattern.clause list) : (Value.t, Ast.error) result =
  let check_res = Exhaustiveness.check_clauses ~sig_env ~span clauses in
  if not check_res.is_exhaustive then
    match check_res.missing_witness with
    | Some w ->
        Error
          Ast.
            {
              span;
              message =
                Printf.sprintf "Non-exhaustive pattern match. Example unhandled case: %s"
                  (Pattern.to_string w);
            }
    | None -> Error Ast.{ span; message = "Non-exhaustive pattern match" }
  else
    let dummy_target = Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 0)) in
    let* tree = compile ~sig_env ~span ~target_expr:dummy_target clauses in
    eval_tree ~fuel env root_val tree

let rec lower_to_core ~span ~target_var (tree : decision_tree) :
    (Core_ir.expr, Ast.error) result =
  let target_node = Core_ir.make_expr ~span (Core_ir.Var target_var) in
  match tree with
  | Leaf act ->
      let curried_body =
        List.fold_left
          (fun acc_body (vname, p) ->
            if String.equal (path_to_string p) "$" then
              Core_ir.make_expr ~span:act.span
                (Core_ir.Let
                   { name = vname; is_rec = false; value = target_node; body = acc_body })
            else acc_body)
          act.body act.bindings
      in
      Ok curried_body
  | SwitchBool { true_branch; false_branch; span = s; _ } ->
      let* tb = lower_to_core ~span:s ~target_var true_branch in
      let* fb = lower_to_core ~span:s ~target_var false_branch in
      Ok
        (Core_ir.make_expr ~span:s
           (Core_ir.If { cond = target_node; then_branch = tb; else_branch = fb }))
  | SwitchLit { cases; default; span = s; _ } ->
      let* default_expr =
        match default with
        | Some d -> lower_to_core ~span:s ~target_var d
        | None -> Ok (Core_ir.make_expr ~span:s (Core_ir.Lit (Ast.Int (-1))))
      in
      List.fold_right
        (fun (lit, subtree) acc_res ->
          let* acc = acc_res in
          let* sub_expr = lower_to_core ~span:s ~target_var subtree in
          let lit_node = Core_ir.make_expr ~span:s (Core_ir.Lit lit) in
          let eq_cond =
            Core_ir.make_expr ~span:s
              (Core_ir.PrimOp { op = Core_ir.Eq; args = [ target_node; lit_node ] })
          in
          Ok
            (Core_ir.make_expr ~span:s
               (Core_ir.If { cond = eq_cond; then_branch = sub_expr; else_branch = acc })))
        cases (Ok default_expr)
  | Guard { bindings; condition; then_tree; else_tree } ->
      let* tb = lower_to_core ~span ~target_var then_tree in
      let* fb = lower_to_core ~span ~target_var else_tree in
      let wrapped_cond =
        List.fold_left
          (fun acc (vname, p) ->
            if String.equal (path_to_string p) "$" then
              Core_ir.make_expr ~span:condition.span
                (Core_ir.Let
                   { name = vname; is_rec = false; value = target_node; body = acc })
            else acc)
          condition bindings
      in
      Ok
        (Core_ir.make_expr ~span
           (Core_ir.If { cond = wrapped_cond; then_branch = tb; else_branch = fb }))
  | SwitchConstructor _ | Failure _ ->
      Ok (Core_ir.make_expr ~span (Core_ir.Lit (Ast.Int 0)))
