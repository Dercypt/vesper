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

type expr = { span : span; desc : expr_desc }
(** Expression AST node preserving source provenance. *)

and expr_desc = Lit of literal | Var of string

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

val validate_expr : expr -> (unit, error) result
(** Recursively validates an expression tree for invariant compliance. *)
