type span = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

type error = { span : span; message : string }
type literal = Int of int | Bool of bool | String of string
type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | And | Or

let binop_to_string = function
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
  | And -> "&&"
  | Or -> "||"

type unop = Neg | Not

let unop_to_string = function Neg -> "-" | Not -> "!"

type expr = { span : span; desc : expr_desc }

and expr_desc =
  | Lit of literal
  | Var of string
  | Unary of { op : unop; arg : expr }
  | Binary of { op : binop; lhs : expr; rhs : expr }
  | If of { cond : expr; then_branch : expr; else_branch : expr }
  | Let of { name : string; is_rec : bool; args : string list; value : expr; body : expr }
  | Fun of { params : string list; body : expr }
  | App of { fn : expr; arg : expr }

type decl = { span : span; decl_desc : decl_desc }

and decl_desc =
  | LetDecl of { name : string; is_rec : bool; args : string list; value : expr }
  | ExprDecl of expr

type program = { span : span; decls : decl list }

let dummy_span : span =
  { file = "<dummy>"; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }

let span_to_string (s : span) : string =
  Printf.sprintf "%s:%d:%d-%d:%d" s.file s.start_line s.start_col s.end_line s.end_col

let span_is_valid (s : span) : bool =
  s.start_line >= 1 && s.start_col >= 0 && s.end_line >= s.start_line
  && if s.end_line = s.start_line then s.end_col >= s.start_col else s.end_col >= 0

let create_span ~file ~start_line ~start_col ~end_line ~end_col : (span, error) result =
  let candidate = { file; start_line; start_col; end_line; end_col } in
  if span_is_valid candidate then Ok candidate
  else
    Error
      {
        span = dummy_span;
        message =
          Printf.sprintf "Invalid source coordinates for span: %s"
            (span_to_string candidate);
      }

let span_of_positions (start_pos, end_pos) : span =
  let file =
    if String.length start_pos.Lexing.pos_fname = 0 then "<input>"
    else start_pos.Lexing.pos_fname
  in
  let start_line = max 1 start_pos.Lexing.pos_lnum in
  let start_col = max 0 (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol) in
  let end_line = max start_line end_pos.Lexing.pos_lnum in
  let raw_end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
  let end_col =
    if end_line = start_line then max start_col raw_end_col else max 0 raw_end_col
  in
  { file; start_line; start_col; end_line; end_col }

let rec validate_expr (e : expr) : (unit, error) result =
  if not (span_is_valid e.span) then
    Error
      {
        span = e.span;
        message =
          Printf.sprintf "AST node violates Law 3 (invalid span provenance): %s"
            (span_to_string e.span);
      }
  else
    match e.desc with
    | Lit (Int _) | Lit (Bool _) | Lit (String _) -> Ok ()
    | Var name ->
        if String.length name = 0 then
          Error { span = e.span; message = "Variable identifier cannot be empty" }
        else Ok ()
    | Unary { op = _; arg } -> validate_expr arg
    | Binary { op = _; lhs; rhs } ->
        let* () = validate_expr lhs in
        validate_expr rhs
    | If { cond; then_branch; else_branch } ->
        let* () = validate_expr cond in
        let* () = validate_expr then_branch in
        validate_expr else_branch
    | Let { name; is_rec = _; args; value; body } ->
        if String.length name = 0 then
          Error { span = e.span; message = "Let-binding identifier cannot be empty" }
        else if List.exists (fun arg -> String.length arg = 0) args then
          Error { span = e.span; message = "Let-binding argument cannot be empty" }
        else
          let* () = validate_expr value in
          validate_expr body
    | Fun { params; body } ->
        if List.length params = 0 then
          Error { span = e.span; message = "Lambda must have at least one parameter" }
        else if List.exists (fun param -> String.length param = 0) params then
          Error { span = e.span; message = "Lambda parameter cannot be empty" }
        else validate_expr body
    | App { fn; arg } ->
        let* () = validate_expr fn in
        validate_expr arg

and ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let validate_decl (d : decl) : (unit, error) result =
  if not (span_is_valid d.span) then
    Error
      {
        span = d.span;
        message =
          Printf.sprintf "Declaration violates Law 3 (invalid span provenance): %s"
            (span_to_string d.span);
      }
  else
    match d.decl_desc with
    | LetDecl { name; is_rec = _; args; value } ->
        if String.length name = 0 then
          Error { span = d.span; message = "Let declaration identifier cannot be empty" }
        else if List.exists (fun arg -> String.length arg = 0) args then
          Error { span = d.span; message = "Let declaration argument cannot be empty" }
        else validate_expr value
    | ExprDecl e -> validate_expr e

let rec validate_decls = function
  | [] -> Ok ()
  | d :: ds ->
      let* () = validate_decl d in
      validate_decls ds

let validate_program (p : program) : (unit, error) result =
  if not (span_is_valid p.span) then
    Error
      {
        span = p.span;
        message =
          Printf.sprintf "Program violates Law 3 (invalid span provenance): %s"
            (span_to_string p.span);
      }
  else validate_decls p.decls

let rec expr_equal (e1 : expr) (e2 : expr) : bool =
  match (e1.desc, e2.desc) with
  | Lit (Int i1), Lit (Int i2) -> i1 = i2
  | Lit (Bool b1), Lit (Bool b2) -> b1 = b2
  | Lit (String s1), Lit (String s2) -> String.equal s1 s2
  | Var v1, Var v2 -> String.equal v1 v2
  | Unary u1, Unary u2 -> u1.op = u2.op && expr_equal u1.arg u2.arg
  | Binary b1, Binary b2 ->
      b1.op = b2.op && expr_equal b1.lhs b2.lhs && expr_equal b1.rhs b2.rhs
  | If i1, If i2 ->
      expr_equal i1.cond i2.cond
      && expr_equal i1.then_branch i2.then_branch
      && expr_equal i1.else_branch i2.else_branch
  | Let l1, Let l2 ->
      String.equal l1.name l2.name && l1.is_rec = l2.is_rec
      && List.equal String.equal l1.args l2.args
      && expr_equal l1.value l2.value && expr_equal l1.body l2.body
  | Fun f1, Fun f2 ->
      List.equal String.equal f1.params f2.params && expr_equal f1.body f2.body
  | App a1, App a2 -> expr_equal a1.fn a2.fn && expr_equal a1.arg a2.arg
  | _ -> false

let decl_equal (d1 : decl) (d2 : decl) : bool =
  match (d1.decl_desc, d2.decl_desc) with
  | LetDecl l1, LetDecl l2 ->
      String.equal l1.name l2.name && l1.is_rec = l2.is_rec
      && List.equal String.equal l1.args l2.args
      && expr_equal l1.value l2.value
  | ExprDecl e1, ExprDecl e2 -> expr_equal e1 e2
  | _ -> false

let ast_equal (p1 : program) (p2 : program) : bool =
  List.equal decl_equal p1.decls p2.decls
