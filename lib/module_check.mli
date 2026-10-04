(** Multi-File Compilation Pipeline, Interface Conformance Checking, and Encapsulation.
    Guarantees Law 1 (Total Diagnosability & Host Isolation), Law 3 (Strict Source
    Provenance), and Law 5 (Deterministic Evaluation & Termination). *)

type checked_module = {
  name : string;
  file_path : string;
  namespace : Namespace.t;
  public_namespace : Namespace.t;
  interface : Mod_ast.mod_sig option;
  span : Ast.span;
}
(** Verified compilation module containing full and public encapsulated namespaces. *)

type checked_project = {
  modules : checked_module list;
  build_order : string list;
  root_env : Namespace.t;
}
(** Complete multi-file project checked in dependency order. *)

val check_interface_conformance :
  impl_ns:Namespace.t -> interface:Mod_ast.mod_sig -> (unit, Diagnostic.t list) result
(** Checks that an implementation namespace satisfies all declarations in an interface
    contract. Returns descriptive diagnostics with error code E4003 upon missing items or
    type mismatches. *)

val check_module :
  ?project_env:Namespace.t ->
  ?interface:Mod_ast.mod_sig ->
  Mod_ast.compilation_unit ->
  (checked_module, Diagnostic.t list) result
(** Checks a single compilation unit against optional project dependencies and interface
    contract. *)

val check_project :
  Mod_ast.compilation_unit list -> (checked_project, Diagnostic.t list) result
(** Validates and compiles a collection of compilation units in dependency order. Performs
    cyclic dependency detection (E4001) and interface conformance verification (E4003). *)

val compile_files : string list -> (checked_project, Diagnostic.t list) result
(** Scans, parses, and compiles multiple source and interface files from disk. *)
