let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let rec lower_expr (e : Ast.expr) : (Core_ir.expr, Ast.error) result =
  if not (Ast.span_is_valid e.span) then
    Error
      {
        span = e.span;
        message =
          Printf.sprintf "AST expression violates Law 3 (invalid span): %s"
            (Ast.span_to_string e.span);
      }
  else
    match e.desc with
    | Lit lit -> Ok (Core_ir.make_expr ~span:e.span (Lit lit))
    | Var name ->
        if String.length name = 0 then
          Error { span = e.span; message = "Variable identifier cannot be empty" }
        else Ok (Core_ir.make_expr ~span:e.span (Var name))
    | Unary { op; arg } ->
        let* arg' = lower_expr arg in
        let core_op = match op with Ast.Neg -> Core_ir.Neg | Ast.Not -> Core_ir.Not in
        Ok (Core_ir.make_expr ~span:e.span (PrimOp { op = core_op; args = [ arg' ] }))
    | Binary { op = And; lhs; rhs } ->
        let* lhs' = lower_expr lhs in
        let* rhs' = lower_expr rhs in
        let false_node = Core_ir.make_expr ~span:rhs'.span (Lit (Bool false)) in
        Ok
          (Core_ir.make_expr ~span:e.span
             (If { cond = lhs'; then_branch = rhs'; else_branch = false_node }))
    | Binary { op = Or; lhs; rhs } ->
        let* lhs' = lower_expr lhs in
        let* rhs' = lower_expr rhs in
        let true_node = Core_ir.make_expr ~span:lhs'.span (Lit (Bool true)) in
        Ok
          (Core_ir.make_expr ~span:e.span
             (If { cond = lhs'; then_branch = true_node; else_branch = rhs' }))
    | Binary { op; lhs; rhs } ->
        let* lhs' = lower_expr lhs in
        let* rhs' = lower_expr rhs in
        let core_op =
          match op with
          | Add -> Core_ir.Add
          | Sub -> Core_ir.Sub
          | Mul -> Core_ir.Mul
          | Div -> Core_ir.Div
          | Mod -> Core_ir.Mod
          | Eq -> Core_ir.Eq
          | Neq -> Core_ir.Neq
          | Lt -> Core_ir.Lt
          | Le -> Core_ir.Le
          | Gt -> Core_ir.Gt
          | Ge -> Core_ir.Ge
          | And | Or -> assert false
        in
        Ok
          (Core_ir.make_expr ~span:e.span
             (PrimOp { op = core_op; args = [ lhs'; rhs' ] }))
    | If { cond; then_branch; else_branch } ->
        let* cond' = lower_expr cond in
        let* then_branch' = lower_expr then_branch in
        let* else_branch' = lower_expr else_branch in
        Ok
          (Core_ir.make_expr ~span:e.span
             (If { cond = cond'; then_branch = then_branch'; else_branch = else_branch' }))
    | Fun { params; body } ->
        if List.length params = 0 then
          Error { span = e.span; message = "Lambda must have at least one parameter" }
        else if List.exists (fun p -> String.length p = 0) params then
          Error { span = e.span; message = "Lambda parameter cannot be empty" }
        else
          let* body' = lower_expr body in
          let curried =
            List.fold_right
              (fun param acc ->
                Core_ir.make_expr ~span:e.span (Fun { param; body = acc }))
              params body'
          in
          Ok curried
    | Let { name; is_rec; args; value; body } ->
        if String.length name = 0 then
          Error { span = e.span; message = "Let-binding identifier cannot be empty" }
        else if List.exists (fun a -> String.length a = 0) args then
          Error { span = e.span; message = "Let-binding argument cannot be empty" }
        else
          let* value' = lower_expr value in
          let* body' = lower_expr body in
          let curried_value =
            List.fold_right
              (fun param acc ->
                Core_ir.make_expr ~span:value.span (Fun { param; body = acc }))
              args value'
          in
          Ok
            (Core_ir.make_expr ~span:e.span
               (Let { name; is_rec; value = curried_value; body = body' }))
    | App { fn; arg } ->
        let* fn' = lower_expr fn in
        let* arg' = lower_expr arg in
        Ok (Core_ir.make_expr ~span:e.span (App { fn = fn'; arg = arg' }))

let lower_decl (d : Ast.decl) : (Core_ir.decl, Ast.error) result =
  if not (Ast.span_is_valid d.span) then
    Error
      {
        span = d.span;
        message =
          Printf.sprintf "AST declaration violates Law 3 (invalid span): %s"
            (Ast.span_to_string d.span);
      }
  else
    match d.decl_desc with
    | LetDecl { name; is_rec; args; value } ->
        if String.length name = 0 then
          Error { span = d.span; message = "Let declaration identifier cannot be empty" }
        else if List.exists (fun a -> String.length a = 0) args then
          Error { span = d.span; message = "Let declaration argument cannot be empty" }
        else
          let* value' = lower_expr value in
          let curried_value =
            List.fold_right
              (fun param acc ->
                Core_ir.make_expr ~span:value.span (Fun { param; body = acc }))
              args value'
          in
          Ok
            (Core_ir.make_decl ~span:d.span
               (LetDecl { name; is_rec; value = curried_value }))
    | ExprDecl e ->
        let* e' = lower_expr e in
        Ok (Core_ir.make_decl ~span:d.span (ExprDecl e'))

let lower (p : Ast.program) : (Core_ir.program, Ast.error) result =
  if not (Ast.span_is_valid p.span) then
    Error
      {
        span = p.span;
        message =
          Printf.sprintf "AST program violates Law 3 (invalid span): %s"
            (Ast.span_to_string p.span);
      }
  else
    let rec lower_decls acc = function
      | [] -> Ok (Core_ir.make_program ~span:p.span (List.rev acc))
      | d :: rest ->
          let* d' = lower_decl d in
          lower_decls (d' :: acc) rest
    in
    lower_decls [] p.decls
