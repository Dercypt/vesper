(** Algebraic Data Types, Record Declarations, and Pattern AST Representation. Preserves
    Law 1 (Total Diagnosability) and Law 3 (Strict Source Provenance). *)

type type_decl = {
  name : string;
  params : string list;
  variants : (string * Types.t list) list;
}
(** Algebraic sum type declaration with variants and type parameter names. *)

type record_decl = {
  name : string;
  params : string list;
  fields : (string * Types.t) list;
}
(** Record product type declaration with named fields and types. *)

type constructor_info = {
  name : string;
  type_name : string;
  type_params : string list;
  arg_types : Types.t list;
}
(** Resolved constructor signature metadata. *)

type pattern = { span : Ast.span; desc : desc }
(** Pattern AST node preserving source provenance. *)

and desc =
  | PWildcard
  | PVar of string
  | PLit of Ast.literal
  | PConstructor of string * pattern list
  | PRecord of (string * pattern) list
  | PTuple of pattern list
  | POr of pattern * pattern
  | PAlias of pattern * string

type clause = {
  span : Ast.span;
  pattern : pattern;
  guard : Core_ir.expr option;
  body : Core_ir.expr;
}
(** Match clause containing a pattern, optional guard expression, and target body. *)

val wildcard : span:Ast.span -> pattern
val var : span:Ast.span -> string -> pattern
val lit : span:Ast.span -> Ast.literal -> pattern
val int : span:Ast.span -> int -> pattern
val bool : span:Ast.span -> bool -> pattern
val string : span:Ast.span -> string -> pattern
val construct : span:Ast.span -> string -> pattern list -> pattern
val record : span:Ast.span -> (string * pattern) list -> pattern
val tuple : span:Ast.span -> pattern list -> pattern
val or_pat : span:Ast.span -> pattern -> pattern -> pattern
val alias : span:Ast.span -> pattern -> string -> pattern

val make_clause :
  span:Ast.span -> ?guard:Core_ir.expr -> pattern -> Core_ir.expr -> clause

val make_type_decl :
  name:string -> ?params:string list -> (string * Types.t list) list -> type_decl

val make_record_decl :
  name:string -> ?params:string list -> (string * Types.t) list -> record_decl

val validate : pattern -> (unit, Ast.error) result
(** Recursively checks span validity (Law 3) and pattern structural invariants. *)

val validate_clause : clause -> (unit, Ast.error) result
val validate_type_decl : type_decl -> (unit, Ast.error) result
val validate_record_decl : record_decl -> (unit, Ast.error) result

val bound_vars : pattern -> string list
(** Returns variable identifiers bound by this pattern in left-to-right order. *)

val equal : pattern -> pattern -> bool
(** Compares two patterns for structural equivalence modulo source spans. *)

val clause_equal : clause -> clause -> bool
val type_decl_equal : type_decl -> type_decl -> bool
val record_decl_equal : record_decl -> record_decl -> bool
val to_string : pattern -> string
val pp : Format.formatter -> pattern -> unit
val type_decl_to_string : type_decl -> string
val pp_type_decl : Format.formatter -> type_decl -> unit
val record_decl_to_string : record_decl -> string
val pp_record_decl : Format.formatter -> record_decl -> unit

type signature_env
(** Immutable registry mapping constructor and type names to declarations. *)

val empty_sig_env : signature_env
val register_type_decl : type_decl -> signature_env -> signature_env
val register_record_decl : record_decl -> signature_env -> signature_env
val lookup_type : string -> signature_env -> type_decl option
val lookup_record : string -> signature_env -> record_decl option
val lookup_constructor : string -> signature_env -> constructor_info option
val constructors_of_type : string -> signature_env -> (string * int) list option

val standard_sig_env : signature_env
(** Standard environment pre-loaded with Option, Result, List, and Bool constructors. *)
