let fold_primop ~span (op : Core_ir.primop) (args : Core_ir.expr list) : Core_ir.expr =
  match (op, args) with
  | Neg, [ { desc = Lit (Int n); _ } ] -> Core_ir.make_expr ~span (Lit (Int (-n)))
  | Not, [ { desc = Lit (Bool b); _ } ] -> Core_ir.make_expr ~span (Lit (Bool (not b)))
  | Add, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Int (a + b)))
  | Sub, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Int (a - b)))
  | Mul, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Int (a * b)))
  | Div, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] when b <> 0 ->
      Core_ir.make_expr ~span (Lit (Int (a / b)))
  | Mod, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] when b <> 0 ->
      Core_ir.make_expr ~span (Lit (Int (a mod b)))
  | Eq, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a = b)))
  | Eq, [ { desc = Lit (Bool a); _ }; { desc = Lit (Bool b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a = b)))
  | Eq, [ { desc = Lit (String a); _ }; { desc = Lit (String b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (String.equal a b)))
  | Neq, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a <> b)))
  | Neq, [ { desc = Lit (Bool a); _ }; { desc = Lit (Bool b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a <> b)))
  | Neq, [ { desc = Lit (String a); _ }; { desc = Lit (String b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (not (String.equal a b))))
  | Lt, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a < b)))
  | Le, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a <= b)))
  | Gt, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a > b)))
  | Ge, [ { desc = Lit (Int a); _ }; { desc = Lit (Int b); _ } ] ->
      Core_ir.make_expr ~span (Lit (Bool (a >= b)))
  | _ -> Core_ir.make_expr ~span (PrimOp { op; args })

let rec normalize_expr (e : Core_ir.expr) : Core_ir.expr =
  match e.desc with
  | Lit _ | Var _ -> e
  | Fun { param; body } ->
      let body' = normalize_expr body in
      Core_ir.make_expr ~span:e.span (Fun { param; body = body' })
  | App { fn; arg } ->
      let fn' = normalize_expr fn in
      let arg' = normalize_expr arg in
      Core_ir.make_expr ~span:e.span (App { fn = fn'; arg = arg' })
  | Let { name; is_rec; value; body } ->
      let value' = normalize_expr value in
      let body' = normalize_expr body in
      Core_ir.make_expr ~span:e.span (Let { name; is_rec; value = value'; body = body' })
  | If { cond; then_branch; else_branch } -> (
      let cond' = normalize_expr cond in
      let then_branch' = normalize_expr then_branch in
      let else_branch' = normalize_expr else_branch in
      match cond'.desc with
      | Lit (Bool true) -> then_branch'
      | Lit (Bool false) -> else_branch'
      | _ ->
          Core_ir.make_expr ~span:e.span
            (If { cond = cond'; then_branch = then_branch'; else_branch = else_branch' }))
  | PrimOp { op; args } ->
      let args' = List.map normalize_expr args in
      fold_primop ~span:e.span op args'

let normalize_decl (d : Core_ir.decl) : Core_ir.decl =
  match d.desc with
  | LetDecl { name; is_rec; value } ->
      Core_ir.make_decl ~span:d.span
        (LetDecl { name; is_rec; value = normalize_expr value })
  | ExprDecl e -> Core_ir.make_decl ~span:d.span (ExprDecl (normalize_expr e))

let normalize (p : Core_ir.program) : Core_ir.program =
  Core_ir.make_program ~span:p.span (List.map normalize_decl p.decls)
