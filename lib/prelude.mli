(** Standard library prelude ingestion and environment pre-population. Guarantees Law 3
    (Source Provenance) and Law 5 (Deterministic Evaluation). *)

val prelude_source : string
(** Raw Vesper source code of the standard prelude. *)

val prelude_ast : unit -> Ast.program
(** Parsed AST representation of the standard prelude. *)

val prelude_core : unit -> Core_ir.program
(** Lowered Core IR representation of the standard prelude. *)

val default_type_env : unit -> Type_env.t
(** Pre-populated typing environment containing signatures and polytypes for standard
    library prelude functions. *)

val default_eval_env : unit -> Env.t
(** Pre-populated runtime evaluation environment containing closure bindings for standard
    library prelude functions. *)

val with_prelude : Ast.program -> Ast.program
(** Prepends the prelude declaration suite to a user compilation unit. *)

val with_prelude_core : Core_ir.program -> Core_ir.program
(** Prepends the prelude Core IR declarations to a user Core IR program. *)
