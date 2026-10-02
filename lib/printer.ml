open Ast

let binop_prec = function
  | Or -> 1
  | And -> 2
  | Eq | Neq -> 3
  | Lt | Le | Gt | Ge -> 4
  | Add | Sub -> 5
  | Mul | Div | Mod -> 6

let unop_prec = function Neg | Not -> 7
let app_prec = 8
let atomic_prec = 9

let prec_of_expr (e : expr) =
  match e.desc with
  | Let _ | If _ | Fun _ -> 0
  | Binary { op; _ } -> binop_prec op
  | Unary { op; _ } -> unop_prec op
  | App _ -> app_prec
  | Lit (Int n) when n < 0 -> 7
  | Lit _ | Var _ -> atomic_prec

let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let rec expr_to_string_with_prec (min_prec : int) (e : expr) : string =
  let p = prec_of_expr e in
  let s =
    match e.desc with
    | Lit (Int n) -> if n < 0 then Printf.sprintf "(%d)" n else string_of_int n
    | Lit (Bool b) -> string_of_bool b
    | Lit (String str) -> escape_string str
    | Var name -> name
    | Unary { op; arg } ->
        let op_str = unop_to_string op in
        let arg_prec =
          match arg.desc with
          | Unary _ -> unop_prec op + 1
          | Lit (Int n) when n < 0 -> unop_prec op + 1
          | _ -> unop_prec op
        in
        let arg_str = expr_to_string_with_prec arg_prec arg in
        Printf.sprintf "%s%s" op_str arg_str
    | Binary { op; lhs; rhs } ->
        let op_p = binop_prec op in
        let lhs_str = expr_to_string_with_prec op_p lhs in
        let rhs_str = expr_to_string_with_prec (op_p + 1) rhs in
        Printf.sprintf "%s %s %s" lhs_str (binop_to_string op) rhs_str
    | If { cond; then_branch; else_branch } ->
        let cond_str = expr_to_string_with_prec 1 cond in
        let then_str =
          match then_branch.desc with
          | If _ -> "(" ^ expr_to_string_with_prec 0 then_branch ^ ")"
          | _ -> expr_to_string_with_prec 0 then_branch
        in
        let else_str = expr_to_string_with_prec 0 else_branch in
        Printf.sprintf "if %s then %s else %s" cond_str then_str else_str
    | Let { name; is_rec; args; value; body } ->
        let rec_str = if is_rec then " rec" else "" in
        let args_str =
          if List.length args = 0 then "" else " " ^ String.concat " " args
        in
        let value_str =
          match value.desc with
          | Let _ | Fun _ | If _ -> "(" ^ expr_to_string_with_prec 0 value ^ ")"
          | _ -> expr_to_string_with_prec 0 value
        in
        let body_str = expr_to_string_with_prec 0 body in
        Printf.sprintf "let%s %s%s = %s in %s" rec_str name args_str value_str body_str
    | Fun { params; body } ->
        let params_str = String.concat " " params in
        let body_str = expr_to_string_with_prec 0 body in
        Printf.sprintf "fun %s -> %s" params_str body_str
    | App { fn; arg } ->
        let fn_str = expr_to_string_with_prec app_prec fn in
        let arg_str = expr_to_string_with_prec atomic_prec arg in
        Printf.sprintf "%s %s" fn_str arg_str
  in
  if p < min_prec then "(" ^ s ^ ")" else s

let expr_to_string (e : expr) : string = expr_to_string_with_prec 0 e

let decl_to_string (d : decl) : string =
  match d.decl_desc with
  | LetDecl { name; is_rec; args; value } ->
      let rec_str = if is_rec then " rec" else "" in
      let args_str = if List.length args = 0 then "" else " " ^ String.concat " " args in
      let value_str = expr_to_string value in
      Printf.sprintf "let%s %s%s = %s;" rec_str name args_str value_str
  | ExprDecl e -> Printf.sprintf "%s;" (expr_to_string e)

let to_string (p : program) : string =
  match p.decls with
  | [] -> ""
  | decls ->
      let formatted = List.map decl_to_string decls in
      String.concat "\n" formatted ^ "\n"
