(** Core Intermediate Representation (Core IR) for Vesper. A minimal, explicit
    intermediate language following desugaring and normalization passes. Enforces Law 3
    (Strict Source Provenance) with mandatory span retention on every node. *)

type literal = Ast.literal = Int of int | Bool of bool | String of string
type primop = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | Neg | Not

val primop_to_string : primop -> string
(** Returns the canonical operator symbol for a primitive operator. *)

type desc =
  | Lit of literal
  | Var of string
  | Let of { name : string; is_rec : bool; value : expr; body : expr }
  | Fun of { param : string; body : expr }
  | App of { fn : expr; arg : expr }
  | PrimOp of { op : primop; args : expr list }
  | If of { cond : expr; then_branch : expr; else_branch : expr }

and expr = { span : Ast.span; desc : desc }
(** Expression node in Core IR preserving source provenance. *)

type t = expr
(** Alias for the primary Core IR expression type. *)

type decl_desc =
  | LetDecl of { name : string; is_rec : bool; value : expr }
  | ExprDecl of expr

type decl = { span : Ast.span; desc : decl_desc }
(** Declaration node in Core IR preserving source provenance. *)

type program = { span : Ast.span; decls : decl list }
(** Complete Core IR program module. *)

val make_expr : span:Ast.span -> desc -> expr
(** Smart constructor for a Core IR expression. *)

val make_decl : span:Ast.span -> decl_desc -> decl
(** Smart constructor for a Core IR declaration. *)

val make_program : span:Ast.span -> decl list -> program
(** Smart constructor for a Core IR program. *)

val validate_expr : expr -> (unit, Ast.error) result
(** Recursively validates an expression tree for invariant compliance and span validity
    (Law 3). *)

val validate_decl : decl -> (unit, Ast.error) result
(** Validates a Core IR declaration node for invariant compliance. *)

val validate_program : program -> (unit, Ast.error) result
(** Recursively validates an entire Core IR program for invariant compliance. *)

val expr_equal : expr -> expr -> bool
(** Compares two Core IR expressions for structural equality modulo source span locations.
*)

val decl_equal : decl -> decl -> bool
(** Compares two Core IR declarations for structural equality modulo source span
    locations. *)

val program_equal : program -> program -> bool
(** Compares two Core IR programs for structural equality modulo source span locations. *)

val equal : t -> t -> bool
(** Alias for [expr_equal]. *)

val pp_expr : Format.formatter -> expr -> unit
(** Pretty-printer for Core IR expressions. *)

val pp_program : Format.formatter -> program -> unit
(** Pretty-printer for Core IR programs. *)

val expr_to_string : expr -> string
(** Formats a Core IR expression as a human-readable string. *)

val to_string : program -> string
(** Formats a Core IR program as a human-readable string. *)
