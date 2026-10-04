(** Hierarchical Symbol Namespaces, Qualified Path Resolution, and Visibility
    Encapsulation. Guarantees Law 1 (Total Diagnosability & Host Isolation), Law 3 (Strict
    Source Provenance), and Law 5 (Deterministic Evaluation & Termination). *)

type visibility =
  | Public
  | Private  (** Visibility of an exported symbol in a module namespace. *)

type val_entry = {
  name : string;
  scheme : Types.scheme;
  visibility : visibility;
  span : Ast.span;
}
(** Value binding entry in a module namespace. *)

type type_entry = {
  name : string;
  params : string list;
  manifest : Types.t option;
  visibility : visibility;
  span : Ast.span;
}
(** Type declaration entry in a module namespace. *)

type mod_entry = {
  name : string;
  namespace : t;
  visibility : visibility;
  span : Ast.span;
}
(** Submodule entry in a module namespace. *)

and t
(** Hierarchical symbol namespace table. *)

(** {1 Constructors & Binding} *)

val empty : t
(** The empty namespace. *)

val is_empty : t -> bool
(** Checks whether the namespace contains no local bindings. *)

val bind_val :
  ?visibility:visibility -> name:string -> scheme:Types.scheme -> span:Ast.span -> t -> t
(** Binds a value identifier to a type scheme in the namespace. *)

val bind_type :
  ?visibility:visibility ->
  name:string ->
  ?params:string list ->
  ?manifest:Types.t ->
  span:Ast.span ->
  t ->
  t
(** Binds a type identifier to its specification in the namespace. *)

val bind_module :
  ?visibility:visibility -> name:string -> mod_ns:t -> span:Ast.span -> t -> t
(** Binds a submodule namespace to a module identifier. *)

(** {1 Direct & Lexical Lookups} *)

val lookup_val_direct : string -> t -> val_entry option
(** Looks up a value binding defined directly in this module scope. *)

val lookup_type_direct : string -> t -> type_entry option
(** Looks up a type binding defined directly in this module scope. *)

val lookup_module_direct : string -> t -> mod_entry option
(** Looks up a submodule defined directly in this module scope. *)

val lookup_val : string -> t -> val_entry option
(** Looks up a value binding in this scope or any opened module namespaces (deterministic
    shadowing). *)

val lookup_type : string -> t -> type_entry option
(** Looks up a type binding in this scope or any opened module namespaces. *)

val lookup_module : string -> t -> mod_entry option
(** Looks up a submodule in this scope or any opened module namespaces. *)

(** {1 Qualified Path Resolution (Mini-Goal 7.3, Step 7.3.1 & 7.3.3)} *)

val resolve_val : path:Mod_ast.path -> t -> (val_entry, Ast.error) result
(** Resolves a qualified identifier path (e.g. [A.B.c]) to its value binding. Enforces
    private visibility boundaries: accessing unexported symbols returns a structured
    diagnostic. *)

val resolve_type : path:Mod_ast.path -> t -> (type_entry, Ast.error) result
(** Resolves a qualified type path to its type declaration. *)

val resolve_module : path:Mod_ast.path -> t -> (mod_entry, Ast.error) result
(** Resolves a qualified module path to its submodule entry. *)

(** {1 Open & Shadowing Rules (Mini-Goal 7.3, Step 7.3.4)} *)

val open_module : path:Mod_ast.path -> t -> (t, Ast.error) result
(** Brings all public symbols from the module at [path] into the current scope. Later
    opened modules shadow earlier opened modules, while local definitions always take
    precedence. *)

val open_namespace : t -> into:t -> t
(** Brings all public symbols from source namespace into target namespace. *)

(** {1 Encapsulation & Interface Filtering (Mini-Goal 7.3, Step 7.3.2 & 7.3.3)} *)

val restrict_interface : Mod_ast.mod_sig -> t -> (t, Ast.error) result
(** Restricts the namespace to the public interface declared in [mod_sig]. Symbols not
    present in the signature become private or hidden from external consumers. *)

val export_public_only : t -> t
(** Returns a namespace containing strictly the public symbols of the given namespace. *)

(** {1 Interoperability & Introspection} *)

val to_type_env : t -> Type_env.t
(** Converts all visible value schemes into a Hindley-Milner typing environment. *)

val all_vals : t -> (string * val_entry) list
(** Lists all direct value bindings in deterministic alphabetical order. *)

val all_types : t -> (string * type_entry) list
(** Lists all direct type bindings in deterministic alphabetical order. *)

val all_modules : t -> (string * mod_entry) list
(** Lists all direct submodule bindings in deterministic alphabetical order. *)

val pp : Format.formatter -> t -> unit
(** Pretty-prints the namespace structure. *)

val to_string : t -> string
(** Formats the namespace as a human-readable string. *)
