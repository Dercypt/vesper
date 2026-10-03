type literal = Ast.literal = Int of int | Bool of bool | String of string
type primop = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | Neg | Not

let primop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Neg -> "-"
  | Not -> "!"

type desc =
  | Lit of literal
  | Var of string
  | Let of { name : string; is_rec : bool; value : expr; body : expr }
  | Fun of { param : string; body : expr }
  | App of { fn : expr; arg : expr }
  | PrimOp of { op : primop; args : expr list }
  | If of { cond : expr; then_branch : expr; else_branch : expr }

and expr = { span : Ast.span; desc : desc }

type t = expr

type decl_desc =
  | LetDecl of { name : string; is_rec : bool; value : expr }
  | ExprDecl of expr

type decl = { span : Ast.span; desc : decl_desc }
type program = { span : Ast.span; decls : decl list }

let make_expr ~span (desc : desc) : expr = { span; desc }
let make_decl ~span (desc : decl_desc) : decl = { span; desc }
let make_program ~span decls : program = { span; decls }
let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let rec validate_expr (e : expr) : (unit, Ast.error) result =
  if not (Ast.span_is_valid e.span) then
    Error
      {
        span = e.span;
        message =
          Printf.sprintf "Core IR expression violates Law 3 (invalid span provenance): %s"
            (Ast.span_to_string e.span);
      }
  else
    match e.desc with
    | Lit (Int _) | Lit (Bool _) | Lit (String _) -> Ok ()
    | Var name ->
        if String.length name = 0 then
          Error { span = e.span; message = "Variable identifier cannot be empty" }
        else Ok ()
    | Let { name; is_rec = _; value; body } ->
        if String.length name = 0 then
          Error { span = e.span; message = "Let-binding identifier cannot be empty" }
        else
          let* () = validate_expr value in
          validate_expr body
    | Fun { param; body } ->
        if String.length param = 0 then
          Error { span = e.span; message = "Lambda parameter cannot be empty" }
        else validate_expr body
    | App { fn; arg } ->
        let* () = validate_expr fn in
        validate_expr arg
    | PrimOp { op; args } ->
        let expected_arity =
          match op with
          | Neg | Not -> 1
          | Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge -> 2
        in
        if List.length args <> expected_arity then
          Error
            {
              span = e.span;
              message =
                Printf.sprintf "PrimOp '%s' expects %d arguments but received %d"
                  (primop_to_string op) expected_arity (List.length args);
            }
        else
          let rec check_args = function
            | [] -> Ok ()
            | a :: rest ->
                let* () = validate_expr a in
                check_args rest
          in
          check_args args
    | If { cond; then_branch; else_branch } ->
        let* () = validate_expr cond in
        let* () = validate_expr then_branch in
        validate_expr else_branch

let validate_decl (d : decl) : (unit, Ast.error) result =
  if not (Ast.span_is_valid d.span) then
    Error
      {
        span = d.span;
        message =
          Printf.sprintf
            "Core IR declaration violates Law 3 (invalid span provenance): %s"
            (Ast.span_to_string d.span);
      }
  else
    match d.desc with
    | LetDecl { name; is_rec = _; value } ->
        if String.length name = 0 then
          Error { span = d.span; message = "Let declaration identifier cannot be empty" }
        else validate_expr value
    | ExprDecl e -> validate_expr e

let rec validate_decls = function
  | [] -> Ok ()
  | d :: ds ->
      let* () = validate_decl d in
      validate_decls ds

let validate_program (p : program) : (unit, Ast.error) result =
  if not (Ast.span_is_valid p.span) then
    Error
      {
        span = p.span;
        message =
          Printf.sprintf "Core IR program violates Law 3 (invalid span provenance): %s"
            (Ast.span_to_string p.span);
      }
  else validate_decls p.decls

let rec expr_equal (e1 : expr) (e2 : expr) : bool =
  match (e1.desc, e2.desc) with
  | Lit (Int i1), Lit (Int i2) -> i1 = i2
  | Lit (Bool b1), Lit (Bool b2) -> b1 = b2
  | Lit (String s1), Lit (String s2) -> String.equal s1 s2
  | Var v1, Var v2 -> String.equal v1 v2
  | Let l1, Let l2 ->
      String.equal l1.name l2.name && l1.is_rec = l2.is_rec
      && expr_equal l1.value l2.value && expr_equal l1.body l2.body
  | Fun f1, Fun f2 -> String.equal f1.param f2.param && expr_equal f1.body f2.body
  | App a1, App a2 -> expr_equal a1.fn a2.fn && expr_equal a1.arg a2.arg
  | PrimOp p1, PrimOp p2 -> p1.op = p2.op && List.equal expr_equal p1.args p2.args
  | If i1, If i2 ->
      expr_equal i1.cond i2.cond
      && expr_equal i1.then_branch i2.then_branch
      && expr_equal i1.else_branch i2.else_branch
  | _ -> false

let decl_equal (d1 : decl) (d2 : decl) : bool =
  match (d1.desc, d2.desc) with
  | LetDecl l1, LetDecl l2 ->
      String.equal l1.name l2.name && l1.is_rec = l2.is_rec
      && expr_equal l1.value l2.value
  | ExprDecl e1, ExprDecl e2 -> expr_equal e1 e2
  | _ -> false

let program_equal (p1 : program) (p2 : program) : bool =
  List.equal decl_equal p1.decls p2.decls

let equal = expr_equal

let rec pp_expr (fmt : Format.formatter) (e : expr) : unit =
  match e.desc with
  | Lit (Int n) -> Format.fprintf fmt "%d" n
  | Lit (Bool b) -> Format.fprintf fmt "%b" b
  | Lit (String s) -> Format.fprintf fmt "%S" s
  | Var id -> Format.fprintf fmt "%s" id
  | Let { name; is_rec; value; body } ->
      let rec_kw = if is_rec then " rec" else "" in
      Format.fprintf fmt "(let%s %s = %a in %a)" rec_kw name pp_expr value pp_expr body
  | Fun { param; body } -> Format.fprintf fmt "(fun %s -> %a)" param pp_expr body
  | App { fn; arg } -> Format.fprintf fmt "(%a %a)" pp_expr fn pp_expr arg
  | PrimOp { op; args = [ arg ] } ->
      Format.fprintf fmt "(%s%a)" (primop_to_string op) pp_expr arg
  | PrimOp { op; args = [ lhs; rhs ] } ->
      Format.fprintf fmt "(%a %s %a)" pp_expr lhs (primop_to_string op) pp_expr rhs
  | PrimOp { op; args } ->
      Format.fprintf fmt "%s(%a)" (primop_to_string op)
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        args
  | If { cond; then_branch; else_branch } ->
      Format.fprintf fmt "(if %a then %a else %a)" pp_expr cond pp_expr then_branch
        pp_expr else_branch

let pp_decl (fmt : Format.formatter) (d : decl) : unit =
  match d.desc with
  | LetDecl { name; is_rec; value } ->
      let rec_kw = if is_rec then " rec" else "" in
      Format.fprintf fmt "let%s %s = %a;" rec_kw name pp_expr value
  | ExprDecl e -> Format.fprintf fmt "%a;" pp_expr e

let pp_program (fmt : Format.formatter) (p : program) : unit =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n") pp_decl fmt p.decls

let expr_to_string (e : expr) : string = Format.asprintf "%a" pp_expr e
let to_string (p : program) : string = Format.asprintf "%a" pp_program p
