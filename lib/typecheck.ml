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

type overload_constraint = { ty : Types.t; op : Ast.binop; span : Ast.span }

let default_and_check_constraints (env : Type_env.t) (subst : Types.subst)
    (constraints : overload_constraint list) :
    (Types.subst * overload_constraint list, Ast.error) result =
  let env_ftv = Type_env.ftv env in
  let rec loop cur_subst remaining_rev = function
    | [] ->
        let remaining =
          List.rev_map (fun c -> { c with ty = Types.apply cur_subst c.ty }) remaining_rev
        in
        Ok (cur_subst, remaining)
    | c :: rest -> (
        let resolved = Types.apply cur_subst c.ty in
        match resolved with
        | Types.TyInt | Types.TyString -> loop cur_subst remaining_rev rest
        | Types.TyVar id ->
            if Types.IntSet.mem id env_ftv then
              loop cur_subst ({ c with ty = resolved } :: remaining_rev) rest
            else
              let* s_def = Unify.unify ~span:c.span resolved Types.TyInt in
              let next_subst = Types.compose_subst s_def cur_subst in
              loop next_subst remaining_rev rest
        | other ->
            let op_name =
              match c.op with
              | Ast.Add -> "operator '+'"
              | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> "comparison operator"
              | _ -> "operator"
            in
            Error
              {
                Ast.code = Some Error_code.E2001_type_error;
                span = c.span;
                message =
                  Printf.sprintf
                    "Type error: %s expects integers or strings, but got '%s'" op_name
                    (Types.to_string other);
              })
  in
  loop subst [] constraints

let rec infer_internal (fresh : unit -> Types.t) (env : Type_env.t) (e : Ast.expr) :
    (Types.subst * Types.t * overload_constraint list, Ast.error) result =
  if not (Ast.span_is_valid e.span) then
    Error
      {
        Ast.code = Some Error_code.E1007_invalid_span;
        span = e.span;
        message =
          Printf.sprintf "Expression violates Law 3 (invalid span): %s"
            (Ast.span_to_string e.span);
      }
  else
    match e.desc with
    | Ast.Lit (Int _) -> Ok (Types.empty_subst, Types.TyInt, [])
    | Ast.Lit (Bool _) -> Ok (Types.empty_subst, Types.TyBool, [])
    | Ast.Lit (String _) -> Ok (Types.empty_subst, Types.TyString, [])
    | Ast.Var name -> (
        match Type_env.lookup name env with
        | Some scheme ->
            let ty = Type_env.instantiate fresh scheme in
            Ok (Types.empty_subst, ty, [])
        | None ->
            Error
              {
                Ast.code = Some Error_code.E2001_type_error;
                span = e.span;
                message = Printf.sprintf "Unbound identifier '%s'" name;
              })
    | Ast.Unary { op = Neg; arg } ->
        let* s1, ty_arg, c1 = infer_internal fresh env arg in
        let* s2 = Unify.unify ~span:arg.span (Types.apply s1 ty_arg) Types.TyInt in
        Ok (Types.compose_subst s2 s1, Types.TyInt, c1)
    | Ast.Unary { op = Not; arg } ->
        let* s1, ty_arg, c1 = infer_internal fresh env arg in
        let* s2 = Unify.unify ~span:arg.span (Types.apply s1 ty_arg) Types.TyBool in
        Ok (Types.compose_subst s2 s1, Types.TyBool, c1)
    | Ast.Binary { op; lhs; rhs } ->
        let* s1, ty_lhs, c1 = infer_internal fresh env lhs in
        let env' = Type_env.apply_subst s1 env in
        let* s2, ty_rhs, c2 = infer_internal fresh env' rhs in
        let s12 = Types.compose_subst s2 s1 in
        let ty_lhs' = Types.apply s2 ty_lhs in
        infer_binop ~span:e.span op ty_lhs' ty_rhs s12 lhs.span rhs.span (c1 @ c2)
    | Ast.If { cond; then_branch; else_branch } ->
        let* s1, ty_cond, c1 = infer_internal fresh env cond in
        let* s2 = Unify.unify ~span:cond.span (Types.apply s1 ty_cond) Types.TyBool in
        let s12 = Types.compose_subst s2 s1 in
        let env_then = Type_env.apply_subst s12 env in
        let* s3, ty_then, c2 = infer_internal fresh env_then then_branch in
        let s123 = Types.compose_subst s3 s12 in
        let env_else = Type_env.apply_subst s123 env in
        let* s4, ty_else, c3 = infer_internal fresh env_else else_branch in
        let s1234 = Types.compose_subst s4 s123 in
        let ty_then' = Types.apply s4 ty_then in
        let* s5 = Unify.unify ~span:e.span ty_then' ty_else in
        let final_s = Types.compose_subst s5 s1234 in
        let final_ty = Types.apply s5 ty_else in
        Ok (final_s, final_ty, c1 @ c2 @ c3)
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
          let* s_body, ty_body, c_body = infer_internal fresh env_extended body in
          let fn_ty =
            List.fold_right
              (fun pv acc -> Types.TyArrow (Types.apply s_body pv, acc))
              param_vars (Types.apply s_body ty_body)
          in
          Ok (s_body, fn_ty, c_body)
    | Ast.App { fn; arg } ->
        let res_var = fresh () in
        let* s1, ty_fn, c1 = infer_internal fresh env fn in
        let env' = Type_env.apply_subst s1 env in
        let* s2, ty_arg, c2 = infer_internal fresh env' arg in
        let s12 = Types.compose_subst s2 s1 in
        let ty_fn' = Types.apply s2 ty_fn in
        let* s3 = Unify.unify ~span:e.span ty_fn' (Types.TyArrow (ty_arg, res_var)) in
        let final_s = Types.compose_subst s3 s12 in
        let final_ty = Types.apply s3 res_var in
        Ok (final_s, final_ty, c1 @ c2)
    | Ast.Let { name; is_rec; args; value; body } -> (
        let value_expr =
          if args = [] then value
          else { Ast.span = value.span; desc = Ast.Fun { params = args; body = value } }
        in
        if not is_rec then
          let* s1, ty_val, c_val = infer_internal fresh env value_expr in
          let env1 = Type_env.apply_subst s1 env in
          let* s1_def, c_val_rem = default_and_check_constraints env1 s1 c_val in
          let env1_def = Type_env.apply_subst s1_def env in
          let final_ty_val = Types.apply s1_def ty_val in
          let scheme = Type_env.generalize env1_def final_ty_val in
          let env2 = Type_env.extend name scheme env1_def in
          let* s2, ty_body, c_body = infer_internal fresh env2 body in
          let final_s = Types.compose_subst s2 s1_def in
          Ok (final_s, ty_body, c_val_rem @ c_body)
        else
          match value_expr.desc with
          | Ast.Fun _ ->
              let rec_var = fresh () in
              let env_rec = Type_env.extend name (Types.Forall ([], rec_var)) env in
              let* s1, ty_val, c_val = infer_internal fresh env_rec value_expr in
              let* s2 = Unify.unify ~span:value.span (Types.apply s1 rec_var) ty_val in
              let s12 = Types.compose_subst s2 s1 in
              let env_outer = Type_env.apply_subst s12 env in
              let* s12_def, c_val_rem =
                default_and_check_constraints env_outer s12 c_val
              in
              let final_ty_val = Types.apply s12_def ty_val in
              let env_outer_def = Type_env.apply_subst s12_def env in
              let scheme = Type_env.generalize env_outer_def final_ty_val in
              let env_body = Type_env.extend name scheme env_outer_def in
              let* s3, ty_body, c_body = infer_internal fresh env_body body in
              let final_s = Types.compose_subst s3 s12_def in
              Ok (final_s, ty_body, c_val_rem @ c_body)
          | _ ->
              Error
                {
                  Ast.code = Some Error_code.E2001_type_error;
                  span = value.span;
                  message = "Recursive bindings must bind a function";
                })

and infer_binop ~span (op : Ast.binop) (ty_lhs : Types.t) (ty_rhs : Types.t)
    (s12 : Types.subst) (lhs_span : Ast.span) (rhs_span : Ast.span)
    (constraints : overload_constraint list) :
    (Types.subst * Types.t * overload_constraint list, Ast.error) result =
  match op with
  | Ast.Sub | Ast.Mul | Ast.Div | Ast.Mod ->
      let* s3 = Unify.unify ~span:lhs_span ty_lhs Types.TyInt in
      let s123 = Types.compose_subst s3 s12 in
      let ty_rhs' = Types.apply s3 ty_rhs in
      let* s4 = Unify.unify ~span:rhs_span ty_rhs' Types.TyInt in
      Ok (Types.compose_subst s4 s123, Types.TyInt, constraints)
  | Ast.Add -> (
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      let s123 = Types.compose_subst s3 s12 in
      let operand_ty = Types.apply s3 ty_rhs in
      match operand_ty with
      | Types.TyString -> Ok (s123, Types.TyString, constraints)
      | Types.TyInt -> Ok (s123, Types.TyInt, constraints)
      | Types.TyVar _ ->
          let c = { ty = operand_ty; op = Ast.Add; span } in
          Ok (s123, operand_ty, c :: constraints)
      | other ->
          Error
            {
              Ast.code = Some Error_code.E2001_type_error;
              span;
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
      Ok (Types.compose_subst s4 s123, Types.TyBool, constraints)
  | Ast.Eq | Ast.Neq ->
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      Ok (Types.compose_subst s3 s12, Types.TyBool, constraints)
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
      let* s3 = Unify.unify ~span ty_lhs ty_rhs in
      let s123 = Types.compose_subst s3 s12 in
      let operand_ty = Types.apply s3 ty_rhs in
      match operand_ty with
      | Types.TyString -> Ok (s123, Types.TyBool, constraints)
      | Types.TyInt -> Ok (s123, Types.TyBool, constraints)
      | Types.TyVar _ ->
          let c = { ty = operand_ty; op; span } in
          Ok (s123, Types.TyBool, c :: constraints)
      | other ->
          Error
            {
              Ast.code = Some Error_code.E2001_type_error;
              span;
              message =
                Printf.sprintf
                  "Type error: comparison operator expects integers or strings, but got \
                   '%s'"
                  (Types.to_string other);
            })

let rec check_internal (fresh : unit -> Types.t) (env : Type_env.t) (expr : Ast.expr)
    (expected_ty : Types.t) : (Types.subst * overload_constraint list, Ast.error) result =
  if not (Ast.span_is_valid expr.span) then
    Error
      {
        Ast.code = Some Error_code.E1007_invalid_span;
        span = expr.span;
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
        let* s1, c1 = check_internal fresh env cond Types.TyBool in
        let env_then = Type_env.apply_subst s1 env in
        let expected_then = Types.apply s1 expected_ty in
        let* s2, c2 = check_internal fresh env_then then_branch expected_then in
        let s12 = Types.compose_subst s2 s1 in
        let env_else = Type_env.apply_subst s12 env in
        let expected_else = Types.apply s12 expected_ty in
        let* s3, c3 = check_internal fresh env_else else_branch expected_else in
        Ok (Types.compose_subst s3 s12, c1 @ c2 @ c3)
    | _ ->
        let* s_infer, actual_ty, c_infer = infer_internal fresh env expr in
        let expected_subst = Types.apply s_infer expected_ty in
        let* s_unify = Unify.unify ~span:expr.span expected_subst actual_ty in
        Ok (Types.compose_subst s_unify s_infer, c_infer)

let infer (env : Type_env.t) (expr : Ast.expr) : (Types.subst * Types.t, Ast.error) result
    =
  let fresh = make_var_gen env in
  let* subst, ty, constraints = infer_internal fresh env expr in
  let* final_subst, _ = default_and_check_constraints env subst constraints in
  Ok (final_subst, Types.apply final_subst ty)

let check (env : Type_env.t) (expr : Ast.expr) (expected_ty : Types.t) :
    (Types.subst, Ast.error) result =
  let fresh = make_var_gen env in
  let* subst, constraints = check_internal fresh env expr expected_ty in
  let* final_subst, _ = default_and_check_constraints env subst constraints in
  Ok final_subst

let typecheck_expr ?(env = Type_env.empty) (expr : Ast.expr) :
    (typed_expr, Ast.error) result =
  let* _subst, ty = infer env expr in
  Ok { span = expr.span; desc = expr.desc; ty }

let typecheck_decl ?(env = Type_env.empty) (decl : Ast.decl) :
    (typed_decl * Type_env.t, Ast.error) result =
  let fresh = make_var_gen env in
  if not (Ast.span_is_valid decl.span) then
    Error
      {
        Ast.code = Some Error_code.E1007_invalid_span;
        span = decl.span;
        message =
          Printf.sprintf "Declaration violates Law 3 (invalid span): %s"
            (Ast.span_to_string decl.span);
      }
  else
    match decl.decl_desc with
    | ExprDecl expr ->
        let* subst, ty, constraints = infer_internal fresh env expr in
        let* final_subst, _ = default_and_check_constraints env subst constraints in
        let final_ty = Types.apply final_subst ty in
        let env' = Type_env.apply_subst final_subst env in
        let typed_d =
          {
            span = decl.span;
            desc = TypedExprDecl { span = expr.span; desc = expr.desc; ty = final_ty };
          }
        in
        Ok (typed_d, env')
    | LetDecl { name; is_rec; args; value } -> (
        let value_expr =
          if args = [] then value
          else { Ast.span = value.span; desc = Ast.Fun { params = args; body = value } }
        in
        if not is_rec then
          let* s1, ty_val, c_val = infer_internal fresh env value_expr in
          let env1 = Type_env.apply_subst s1 env in
          let* s1_def, _ = default_and_check_constraints env1 s1 c_val in
          let env1_def = Type_env.apply_subst s1_def env in
          let final_ty = Types.apply s1_def ty_val in
          let scheme = Type_env.generalize env1_def final_ty in
          let env_extended = Type_env.extend name scheme env1_def in
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
          match value_expr.desc with
          | Ast.Fun _ ->
              let rec_var = fresh () in
              let env_rec = Type_env.extend name (Types.Forall ([], rec_var)) env in
              let* s1, ty_val, c_val = infer_internal fresh env_rec value_expr in
              let* s2 = Unify.unify ~span:value.span (Types.apply s1 rec_var) ty_val in
              let s12 = Types.compose_subst s2 s1 in
              let env_outer = Type_env.apply_subst s12 env in
              let* s12_def, _ = default_and_check_constraints env_outer s12 c_val in
              let ty_val' = Types.apply s12_def ty_val in
              let env_outer_def = Type_env.apply_subst s12_def env in
              let scheme = Type_env.generalize env_outer_def ty_val' in
              let env_extended = Type_env.extend name scheme env_outer_def in
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
          | _ ->
              Error
                {
                  Ast.code = Some Error_code.E2001_type_error;
                  span = value.span;
                  message = "Recursive bindings must bind a function";
                })

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
          | Error err ->
              Error [ Diagnostic.of_ast_error ~code:Error_code.E2001_type_error err ])
    in
    check_decls env [] prog.decls

let typecheck prog = typecheck_program prog
