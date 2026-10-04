(** Module Dependency Graph, Fast Import Scanning, Cycle Detection, and Topological
    Sorting. Guarantees Law 1 (Total Diagnosability & Host Isolation), Law 3 (Strict
    Source Provenance), and Law 5 (Deterministic Evaluation & Termination). *)

type import_info = { module_name : string; span : Ast.span }
(** Extracted import information carrying the imported module name and exact source span.
*)

type t
(** Immutable directed dependency graph of compilation units. *)

val empty : t
(** Empty dependency graph. *)

val is_empty : t -> bool
(** Checks whether the dependency graph is empty. *)

val add_node : name:string -> span:Ast.span -> ?file_path:string -> t -> t
(** Adds a compilation unit node to the graph. *)

val add_dependency : from_node:string -> to_node:string -> span:Ast.span -> t -> t
(** Adds a directed dependency edge from [from_node] to [to_node] ([from_node] depends on
    [to_node]). *)

val mem : string -> t -> bool
(** Returns true if the node exists in the dependency graph. *)

val nodes : t -> string list
(** Returns all node names in deterministic sorted order. *)

val node_span : string -> t -> Ast.span option
(** Retrieves the declaration span of a node. *)

val node_name : string -> t -> string option
(** Retrieves the canonical module name of a node if it exists. *)

val node_file : string -> t -> string option
(** Retrieves the file path of a node if known. *)

val dependency_span : from_node:string -> to_node:string -> t -> Ast.span option
(** Retrieves the source span of the dependency edge from [from_node] to [to_node]. *)

val dependencies_of : string -> t -> string list
(** Returns the direct dependencies of a module ([to_node]s it imports). *)

val dependents_of : string -> t -> string list
(** Returns the direct dependents of a module (modules that import it). *)

(** {1 Fast Import Scanner (Mini-Goal 7.2, Step 7.2.1)} *)

val scan_imports_from_string :
  file:string -> string -> (import_info list, Ast.error) result
(** Scans source text for imported module names without running full AST parsing passes.
    Tracks line and column spans accurately (Law 3). *)

val scan_imports_from_file : string -> (import_info list, Ast.error) result
(** Reads a file from disk and extracts imported module names without panics (Law 1). *)

(** {1 Graph Construction} *)

val of_units : Mod_ast.compilation_unit list -> (t, Ast.error) result
(** Constructs a dependency graph from a list of compilation units. *)

val build_from_files : string list -> (t, Ast.error) result
(** Scans a list of source file paths and builds the complete module dependency graph. *)

(** {1 Tarjan SCC & Cycle Detection (Mini-Goal 7.2, Steps 7.2.3 & 7.2.4)} *)

val find_sccs : t -> string list list
(** Computes strongly connected components (SCCs) using Tarjan's algorithm. *)

val find_cycles : t -> string list list
(** Returns all detected cycles, formatted as cycle paths (e.g. [["A"; "B"; "A"]]). *)

val detect_cycles : t -> (unit, Ast.error) result
(** Validates that the graph is a Directed Acyclic Graph (DAG). Returns a structured error
    [Error { span; message = "Cyclic dependency detected: A -> B -> A" }] on cycles. *)

(** {1 Topological Sort & Build Ordering (Mini-Goal 7.2, Step 7.2.5)} *)

val topological_sort : t -> (string list, Ast.error) result
(** Computes a deterministic topological sort of compilation units for sequential
    compilation. Dependencies appear strictly before their dependents. *)
