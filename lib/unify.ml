let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let rec occurs_check (var_id : int) (ty : Types.t) : bool =
  match ty with
  | Types.TyVar id -> id = var_id
  | Types.TyArrow (t1, t2) -> occurs_check var_id t1 || occurs_check var_id t2
  | Types.TyInt | Types.TyBool | Types.TyString | Types.TyUnit -> false

let bind_var ~span (var_id : int) (ty : Types.t) : (Types.subst, Ast.error) result =
  match ty with
  | Types.TyVar id when id = var_id -> Ok Types.empty_subst
  | _ when occurs_check var_id ty ->
      Error
        {
          Ast.span;
          message =
            Printf.sprintf
              "Occurs check failed: type variable '%s' occurs within '%s', resulting in \
               an infinite type"
              (Types.to_string (Types.TyVar var_id))
              (Types.to_string ty);
        }
  | _ -> Ok (Types.singleton_subst var_id ty)

let rec unify ~span (t1 : Types.t) (t2 : Types.t) : (Types.subst, Ast.error) result =
  match (t1, t2) with
  | TyInt, TyInt | TyBool, TyBool | TyString, TyString | TyUnit, TyUnit ->
      Ok Types.empty_subst
  | TyVar id1, TyVar id2 when id1 = id2 -> Ok Types.empty_subst
  | TyVar id1, other -> bind_var ~span id1 other
  | other, TyVar id2 -> bind_var ~span id2 other
  | TyArrow (arg1, res1), TyArrow (arg2, res2) ->
      let* s1 = unify ~span arg1 arg2 in
      let res1' = Types.apply s1 res1 in
      let res2' = Types.apply s1 res2 in
      let* s2 = unify ~span res1' res2' in
      Ok (Types.compose_subst s2 s1)
  | expected, actual ->
      Error
        {
          Ast.span;
          message =
            Printf.sprintf "Type mismatch: expected '%s', but got '%s'"
              (Types.to_string expected) (Types.to_string actual);
        }
