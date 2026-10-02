type span = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}
(** Source location span representing source text coordinates. *)

type error = { span : span; message : string }
(** Diagnostic error carrying source span and explanation. *)

(** Literals supported by Vesper. *)
type literal = Int of int | Bool of bool | String of string

(** Binary operators. *)
type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | And | Or

val binop_to_string : binop -> string
(** Returns the canonical operator symbol for a binary operator. *)

(** Unary operators. *)
type unop = Neg | Not

val unop_to_string : unop -> string
(** Returns the canonical operator symbol for a unary operator. *)

type expr = { span : span; desc : expr_desc }
(** Expression AST node preserving source provenance. *)

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
(** Top-level declaration node preserving source provenance. *)

and decl_desc =
  | LetDecl of { name : string; is_rec : bool; args : string list; value : expr }
  | ExprDecl of expr

type program = { span : span; decls : decl list }
(** Complete compilation unit (program). *)

val dummy_span : span
(** A synthetic or empty placeholder span. *)

val span_to_string : span -> string
(** Formats a span as "file:line:col-line:col". *)

val span_is_valid : span -> bool
(** Validates whether a span has non-negative and topologically valid coordinates. *)

val create_span :
  file:string ->
  start_line:int ->
  start_col:int ->
  end_line:int ->
  end_col:int ->
  (span, error) result
(** Safe smart constructor for spans returning a Result type. Enforces Law 1 (no uncaught
    exceptions) and Law 3 (provenance integrity). *)

val span_of_positions : Lexing.position * Lexing.position -> span
(** Constructs a valid source span from a pair of Lexing positions. *)

val validate_expr : expr -> (unit, error) result
(** Recursively validates an expression tree for invariant compliance. *)

val validate_decl : decl -> (unit, error) result
(** Validates a declaration node for invariant compliance. *)

val validate_program : program -> (unit, error) result
(** Recursively validates an entire program AST for invariant compliance. *)

val expr_equal : expr -> expr -> bool
(** Compares two expressions for structural equality modulo source span locations. *)

val decl_equal : decl -> decl -> bool
(** Compares two declarations for structural equality modulo source span locations. *)

val ast_equal : program -> program -> bool
(** Compares two programs for structural equality modulo source span locations (Law 2). *)
