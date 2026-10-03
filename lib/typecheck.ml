let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

type typed_expr = { span : Ast.span; desc : Ast.expr_desc; ty : Types.t }

type typed_decl_desc =
  | TypedLetDecl of {
      name : string;
      is_rec : bool;
      args : string list;
      value : typed_expr;
      scheme : Types.scheme;
    }
  | TypedExprDecl of typed_expr

type typed_decl = { span : Ast.span; desc : typed_decl_desc }
type typed_program = { span : Ast.span; decls : typed_decl list; env : Type_env.t }
type typed_ir = typed_program

let make_var_gen (env : Type_env.t) =
  let counter = ref (Type_env.max_var env + 1) in
  fun () ->
    let id = !counter in
    incr counter;
    Types.TyVar id

let rec infer_internal (fresh : unit -> Types.t) (env : Type_env.t) (e : Ast.expr) :
    (Types.subst * Types.t, Ast.error) result =
  if not (Ast.span_is_valid e.span) then
    Error
      {
        Ast.span = e.span;
        message =
          Printf.sprintf "Expression violates Law 3 (invalid span): %s"
            (Ast.span_to_string e.span);
      }
  else
    match e.desc with
    | Ast.Lit (Int _) -> Ok (Types.empty_subst, Types.TyInt)
    | Ast.Lit (Bool _) -> Ok (Types.empty_subst, Types.TyBool)
    | Ast.Lit (String _) -> Ok (Types.empty_subst, Types.TyString)
    | Ast.Var name -> (
        match Type_env.lookup name env with
        | Some scheme ->
            let ty = Type_env.instantiate fresh scheme in
            Ok (Types.empty_subst, ty)
        | None ->
            Error
              {
                Ast.span = e.span;
                message = Printf.sprintf "Unbound identifier '%s'" name;
              })
    | Ast.Unary { op = Neg; arg } ->
        let* s1, ty_arg = infer_internal fresh env arg in
        let* s2 = Unify.unify ~span:arg.span (Types.apply s1 ty_arg) Types.TyInt in
        Ok (Types.compose_subst s2 s1, Types.TyInt)
    | Ast.Unary { op = Not; arg } ->
        let* s1, ty_arg = infer_internal fresh env arg in
        let* s2 = Unify.unify ~span:arg.span (Types.apply s1 ty_arg) Types.TyBool in
        Ok (Types.compose_subst s2 s1, Types.TyBool)
    | Ast.Binary { op; lhs; rhs } ->
        let* s1, ty_lhs = infer_internal fresh env lhs in
        let env' = Type_env.apply_subst s1 env in
        let* s2, ty_rhs = infer_internal fresh env' rhs in
        let s12 = Types.compose_subst s2 s1 in
        let ty_lhs' = Types.apply s2 ty_lhs in
        infer_binop ~span:e.span op ty_lhs' ty_rhs s12 lhs.span rhs.span
    | Ast.If { cond; then_branch; else_branch } ->
        let* s1, ty_cond = infer_internal fresh env cond in
        let* s2 = Unify.unify ~span:cond.span (Types.apply s1 ty_cond) Types.TyBool in
        let s12 = Types.compose_subst s2 s1 in
        let env_then = Type_env.apply_subst s12 env in
        let* s3, ty_then = infer_internal fresh env_then then_branch in
        let s123 = Types.compose_subst s3 s12 in
        let env_else = Type_env.apply_subst s123 env in
        let* s4, ty_else = infer_internal fresh env_else else_branch in
        let s1234 = Types.compose_subst s4 s123 in
        let ty_then' = Types.apply s4 ty_then in
        let* s5 = Unify.unify ~span:e.span ty_then' ty_else in
        let final_s = Types.compose_subst s5 s1234 in
        let final_ty = Types.apply s5 ty_else in
        Ok (final_s, final_ty)
    | Ast.Fun { params; body } ->
        if params = [] then infer_internal fresh env body
        else
          let param_vars = List.map (fun _ -> fresh ()) params in
          let env_extended =
            List.fold_left2
              (fun cur_env param pty ->
                Type_env.extend param (Types.Forall ([], pty)) cur_env)
              env params param_vars
          in
          let* s_body, ty_body = infer_internal fresh env_extended body in
          let fn_ty =
            List.fold_right
              (fun pv acc -> Types.TyArrow (Types.apply s_body pv, acc))
              param_vars (Types.apply s_body ty_body)
          in
          Ok (s_body, fn_ty)
    | Ast.App { fn; arg } ->
        let res_var = fresh () in
        let* s1, ty_fn = infer_internal fresh env fn in
        let env' = Type_env.apply_subst s1 env in
        let* s2, ty_arg = infer_internal fresh env' arg in
        let s12 = Types.compose_subst s2 s1 in
        let ty_fn' = Types.apply s2 ty_fn in
        let* s3 = Unify.unify ~span:e.span ty_fn' (Types.TyArrow (ty_arg, res_var)) in
        let final_s = Types.compose_subst s3 s12 in
        let final_ty = Types.apply s3 res_var in
        Ok (final_s, final_ty)
    | Ast.Let { name; is_rec; args; value; body } ->
        let value_expr =
          if args = [] then value
          else { Ast.span = value.span; desc = Ast.Fun { params = args; body = value } }
        in
        if not is_rec then
          let* s1, ty_val = infer_internal fresh env value_expr in
          let env1 = Type_env.apply_subst s1 env in
          let final_ty_val = Types.apply s1 ty_val in
          let scheme = Type_env.generalize env1 final_ty_val in
          let env2 = Type_env.extend name scheme env1 in
          let* s2, ty_body = infer_internal fresh env2 body in
          Ok (Types.compose_subst s2 s1, ty_body)
        else
          let rec_var = fresh () in
          let env_rec = Type_env.extend name (Types.Forall ([], rec_var)) env in
          let* s1, ty_val = infer_internal fresh env_rec value_expr in
          let* s2 = Unify.unify ~span:value.span (Types.apply s1 rec_var) ty_val in
          let s12 = Types.compose_subst s2 s1 in
          let final_ty_val = Types.apply s2 ty_val in
          let env_outer = Type_env.apply_subst s12 env in
          let scheme = Type_env.generalize env_outer final_ty_val in
          let env_body = Type_env.extend name scheme env_outer in
          let* s3, ty_body = infer_internal fresh env_body body in
          Ok (Types.compose_subst s3 s12, ty_body)

and infer_binop ~span (op : Ast.binop) (ty_lhs : Types.t) (ty_rhs : Types.t)
    (s12 : Types.subst) (lhs_span : Ast.span) (rhs_span : Ast.span) :
    (Types.subst * Types.t, Ast.error) result =
  match op with
  | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
      let* s3 = Unify.unify ~span:lhs_span ty_lhs Types.TyInt in
      let s123 = Types.compose_subst s3 s12 in
      let ty_rhs' = Types.apply s3 ty_rhs in
      let* s4 = Unify.unify ~span:rhs_span ty_rhs' Types.TyInt in
      Ok (Types.compose_subst s4 s123, Types.TyInt)
  | Ast.Add -> (
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      let s123 = Types.compose_subst s3 s12 in
      let operand_ty = Types.apply s3 ty_rhs in
      match operand_ty with
      | Types.TyString -> Ok (s123, Types.TyString)
      | Types.TyInt -> Ok (s123, Types.TyInt)
      | Types.TyVar _ ->
          let* s4 = Unify.unify ~span operand_ty Types.TyInt in
          Ok (Types.compose_subst s4 s123, Types.TyInt)
      | other ->
          Error
            {
              Ast.span;
              message =
                Printf.sprintf
                  "Type error: operator '+' expects integers or strings, but got '%s'"
                  (Types.to_string other);
            })
  | Ast.And | Ast.Or ->
      let* s3 = Unify.unify ~span:lhs_span ty_lhs Types.TyBool in
      let s123 = Types.compose_subst s3 s12 in
      let ty_rhs' = Types.apply s3 ty_rhs in
      let* s4 = Unify.unify ~span:rhs_span ty_rhs' Types.TyBool in
      Ok (Types.compose_subst s4 s123, Types.TyBool)
  | Ast.Eq | Ast.Neq ->
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      Ok (Types.compose_subst s3 s12, Types.TyBool)
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      let s123 = Types.compose_subst s3 s12 in
      let operand_ty = Types.apply s3 ty_rhs in
      match operand_ty with
      | Types.TyString -> Ok (s123, Types.TyBool)
      | Types.TyInt -> Ok (s123, Types.TyBool)
      | Types.TyVar _ ->
          let* s4 = Unify.unify ~span operand_ty Types.TyInt in
          Ok (Types.compose_subst s4 s123, Types.TyBool)
      | other ->
          Error
            {
              Ast.span;
              message =
                Printf.sprintf
                  "Type error: comparison operator expects integers or strings, but got \
                   '%s'"
                  (Types.to_string other);
            })

let rec check_internal (fresh : unit -> Types.t) (env : Type_env.t) (expr : Ast.expr)
    (expected_ty : Types.t) : (Types.subst, Ast.error) result =
  if not (Ast.span_is_valid expr.span) then
    Error
      {
        Ast.span = expr.span;
        message =
          Printf.sprintf "Expression violates Law 3 (invalid span): %s"
            (Ast.span_to_string expr.span);
      }
  else
    match (expr.desc, expected_ty) with
    | Ast.Fun { params = p :: rest; body }, Types.TyArrow (arg_ty, res_ty) ->
        let env' = Type_env.extend p (Types.Forall ([], arg_ty)) env in
        let next_expr =
          if rest = [] then body
          else { Ast.span = expr.span; desc = Ast.Fun { params = rest; body } }
        in
        check_internal fresh env' next_expr res_ty
    | Ast.If { cond; then_branch; else_branch }, _ ->
        let* s1 = check_internal fresh env cond Types.TyBool in
        let env_then = Type_env.apply_subst s1 env in
        let expected_then = Types.apply s1 expected_ty in
        let* s2 = check_internal fresh env_then then_branch expected_then in
        let s12 = Types.compose_subst s2 s1 in
        let env_else = Type_env.apply_subst s12 env in
        let expected_else = Types.apply s12 expected_ty in
        let* s3 = check_internal fresh env_else else_branch expected_else in
        Ok (Types.compose_subst s3 s12)
    | _ ->
        let* s_infer, actual_ty = infer_internal fresh env expr in
        let expected_subst = Types.apply s_infer expected_ty in
        let* s_unify = Unify.unify ~span:expr.span expected_subst actual_ty in
        Ok (Types.compose_subst s_unify s_infer)

let infer (env : Type_env.t) (expr : Ast.expr) : (Types.subst * Types.t, Ast.error) result
    =
  let fresh = make_var_gen env in
  infer_internal fresh env expr

let check (env : Type_env.t) (expr : Ast.expr) (expected_ty : Types.t) :
    (Types.subst, Ast.error) result =
  let fresh = make_var_gen env in
  check_internal fresh env expr expected_ty

let typecheck_expr ?(env = Type_env.empty) (expr : Ast.expr) :
    (typed_expr, Ast.error) result =
  let* subst, ty = infer env expr in
  let final_ty = Types.apply subst ty in
  Ok { span = expr.span; desc = expr.desc; ty = final_ty }

let typecheck_decl ?(env = Type_env.empty) (decl : Ast.decl) :
    (typed_decl * Type_env.t, Ast.error) result =
  let fresh = make_var_gen env in
  if not (Ast.span_is_valid decl.span) then
    Error
      {
        Ast.span = decl.span;
        message =
          Printf.sprintf "Declaration violates Law 3 (invalid span): %s"
            (Ast.span_to_string decl.span);
      }
  else
    match decl.decl_desc with
    | ExprDecl expr ->
        let* subst, ty = infer_internal fresh env expr in
        let final_ty = Types.apply subst ty in
        let env' = Type_env.apply_subst subst env in
        let typed_d =
          {
            span = decl.span;
            desc = TypedExprDecl { span = expr.span; desc = expr.desc; ty = final_ty };
          }
        in
        Ok (typed_d, env')
    | LetDecl { name; is_rec; args; value } ->
        let value_expr =
          if args = [] then value
          else { Ast.span = value.span; desc = Ast.Fun { params = args; body = value } }
        in
        if not is_rec then
          let* s1, ty_val = infer_internal fresh env value_expr in
          let env1 = Type_env.apply_subst s1 env in
          let final_ty = Types.apply s1 ty_val in
          let scheme = Type_env.generalize env1 final_ty in
          let env_extended = Type_env.extend name scheme env1 in
          let typed_d =
            {
              span = decl.span;
              desc =
                TypedLetDecl
                  {
                    name;
                    is_rec;
                    args;
                    value = { span = value.span; desc = value.desc; ty = final_ty };
                    scheme;
                  };
            }
          in
          Ok (typed_d, env_extended)
        else
          let rec_var = fresh () in
          let env_rec = Type_env.extend name (Types.Forall ([], rec_var)) env in
          let* s1, ty_val = infer_internal fresh env_rec value_expr in
          let* s2 = Unify.unify ~span:value.span (Types.apply s1 rec_var) ty_val in
          let s12 = Types.compose_subst s2 s1 in
          let ty_val' = Types.apply s2 ty_val in
          let env_outer = Type_env.apply_subst s12 env in
          let scheme = Type_env.generalize env_outer ty_val' in
          let env_extended = Type_env.extend name scheme env_outer in
          let typed_d =
            {
              span = decl.span;
              desc =
                TypedLetDecl
                  {
                    name;
                    is_rec;
                    args;
                    value = { span = value.span; desc = value.desc; ty = ty_val' };
                    scheme;
                  };
            }
          in
          Ok (typed_d, env_extended)

let typecheck_program ?(env = Type_env.empty) (prog : Ast.program) :
    (typed_program, Diagnostic.t list) result =
  if not (Ast.span_is_valid prog.span) then
    let diag =
      Diagnostic.error ~code:Error_code.E1007_invalid_span ~span:prog.span
        (Printf.sprintf "Program violates Law 3 (invalid span): %s"
           (Ast.span_to_string prog.span))
    in
    Error [ diag ]
  else
    let rec check_decls cur_env acc = function
      | [] -> Ok { span = prog.span; decls = List.rev acc; env = cur_env }
      | d :: rest -> (
          match typecheck_decl ~env:cur_env d with
          | Ok (typed_d, next_env) -> check_decls next_env (typed_d :: acc) rest
          | Error err -> Error [ Diagnostic.of_ast_error err ])
    in
    check_decls env [] prog.decls

let typecheck prog = typecheck_program prog
