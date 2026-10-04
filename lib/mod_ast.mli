(** Module System, Namespaces, Interfaces, and Compilation Units AST. Guarantees Law 1
    (Total Diagnosability & Host Isolation), Law 2 (Invertible Syntax), and Law 3 (Strict
    Source Provenance). *)

type path_segment = { name : string; span : Ast.span }
(** An individual segment in a qualified path, carrying its own precise source span. *)

type path = { span : Ast.span; segments : path_segment list }
(** A qualified module or identifier path (e.g. A.B.c) with span tracking across all
    segments. *)

type type_spec = {
  name : string;
  params : string list;
  manifest : Types.t option;
  span : Ast.span;
}
(** Type specification in a module signature. [manifest = None] represents an abstract
    type. *)

type val_spec = { name : string; ty : Types.t; span : Ast.span }
(** Value specification declaring the expected type of an exported symbol in an interface.
*)

type sig_item =
  | SigVal of val_spec
  | SigType of type_spec
  | SigModule of { name : string; signature : mod_sig; span : Ast.span }
  | SigOpen of { path : path; span : Ast.span }
      (** An item inside a module interface or signature. *)

and mod_sig =
  | SigBody of { items : sig_item list; span : Ast.span }
  | SigIdent of { path : path; span : Ast.span }
      (** A module signature (interface contract). *)

type val_def = {
  name : string;
  is_rec : bool;
  args : string list;
  value : Ast.expr;
  annot : Types.t option;
  span : Ast.span;
}
(** Value definition in a module implementation. *)

type type_def = { name : string; params : string list; ty : Types.t; span : Ast.span }
(** Type definition in a module implementation. *)

type mod_item =
  | ModVal of val_def
  | ModType of type_def
  | ModModule of { name : string; expr : mod_expr; span : Ast.span }
  | ModOpen of { path : path; span : Ast.span }
  | ModImport of { path : path; alias : string option; span : Ast.span }
      (** An item inside a module structure or file. *)

and mod_expr =
  | ModStruct of { items : mod_item list; span : Ast.span }
  | ModVar of { path : path; span : Ast.span }
  | ModAscribed of { expr : mod_expr; signature : mod_sig; span : Ast.span }
      (** A module expression. *)

type unit_kind =
  | Implementation of mod_expr
  | Interface of mod_sig
      (** Kind of compilation unit: .vesper implementation or .vesperi interface. *)

type compilation_unit = {
  name : string;
  file_path : string;
  kind : unit_kind;
  span : Ast.span;
}
(** A full compilation unit representing a source or interface file. *)

(** {1 Smart Constructors} *)

val make_segment : span:Ast.span -> string -> path_segment
val make_path : span:Ast.span -> path_segment list -> path
val path_of_strings : span:Ast.span -> string list -> (path, Ast.error) result
val parse_path : span:Ast.span -> string -> (path, Ast.error) result
val make_val_spec : span:Ast.span -> name:string -> ty:Types.t -> val_spec

val make_type_spec :
  span:Ast.span ->
  name:string ->
  ?params:string list ->
  ?manifest:Types.t ->
  unit ->
  type_spec

val make_sig_val : span:Ast.span -> name:string -> ty:Types.t -> sig_item

val make_sig_type :
  span:Ast.span ->
  name:string ->
  ?params:string list ->
  ?manifest:Types.t ->
  unit ->
  sig_item

val make_sig_module : span:Ast.span -> name:string -> mod_sig -> sig_item
val make_sig_open : span:Ast.span -> path -> sig_item
val make_sig_body : span:Ast.span -> sig_item list -> mod_sig
val make_sig_ident : span:Ast.span -> path -> mod_sig

val make_val_def :
  span:Ast.span ->
  name:string ->
  ?is_rec:bool ->
  ?args:string list ->
  ?annot:Types.t ->
  Ast.expr ->
  val_def

val make_type_def :
  span:Ast.span -> name:string -> ?params:string list -> ty:Types.t -> unit -> type_def

val make_mod_val :
  span:Ast.span ->
  name:string ->
  ?is_rec:bool ->
  ?args:string list ->
  ?annot:Types.t ->
  Ast.expr ->
  mod_item

val make_mod_type :
  span:Ast.span -> name:string -> ?params:string list -> ty:Types.t -> unit -> mod_item

val make_mod_module : span:Ast.span -> name:string -> mod_expr -> mod_item
val make_mod_open : span:Ast.span -> path -> mod_item
val make_mod_import : span:Ast.span -> ?alias:string -> path -> mod_item
val make_mod_struct : span:Ast.span -> mod_item list -> mod_expr
val make_mod_var : span:Ast.span -> path -> mod_expr
val make_mod_ascribed : span:Ast.span -> mod_expr -> mod_sig -> mod_expr

val make_unit :
  span:Ast.span -> name:string -> file_path:string -> unit_kind -> compilation_unit

(** {1 Path Operations} *)

val path_to_string : path -> string
val head_segment : path -> (path_segment, Ast.error) result
val last_segment : path -> (path_segment, Ast.error) result
val drop_last : path -> (path * path_segment, Ast.error) result
val append_segment : path -> path_segment -> path

(** {1 Validation Functions (Law 3: Strict Source Provenance)} *)

val validate_path : path -> (unit, Ast.error) result
val validate_sig_item : sig_item -> (unit, Ast.error) result
val validate_sig : mod_sig -> (unit, Ast.error) result
val validate_mod_item : mod_item -> (unit, Ast.error) result
val validate_mod_expr : mod_expr -> (unit, Ast.error) result
val validate_unit : compilation_unit -> (unit, Ast.error) result

(** {1 Structural Equality (Modulo Spans)} *)

val path_equal : path -> path -> bool
val sig_item_equal : sig_item -> sig_item -> bool
val sig_equal : mod_sig -> mod_sig -> bool
val mod_item_equal : mod_item -> mod_item -> bool
val mod_expr_equal : mod_expr -> mod_expr -> bool
val unit_equal : compilation_unit -> compilation_unit -> bool

(** {1 Pretty-Printing & Formatting (Law 2: Invertible Syntax)} *)

val sig_item_to_string : sig_item -> string
val sig_to_string : mod_sig -> string
val mod_item_to_string : mod_item -> string
val mod_expr_to_string : mod_expr -> string
val unit_to_string : compilation_unit -> string
val pp_path : Format.formatter -> path -> unit
val pp_sig : Format.formatter -> mod_sig -> unit
val pp_mod_expr : Format.formatter -> mod_expr -> unit
val pp_unit : Format.formatter -> compilation_unit -> unit

(** {1 AST Conversion & File Parsing} *)

val of_ast_program : ?name:string -> ?file_path:string -> Ast.program -> compilation_unit
val of_ast_decls : Ast.decl list -> mod_item list
val parse_interface : file:string -> string -> (mod_sig, Ast.error) result
val parse_implementation : file:string -> string -> (mod_expr, Ast.error) result
val parse_compilation_unit : file:string -> string -> (compilation_unit, Ast.error) result
