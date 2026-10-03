(** Idempotent normalization and simplification passes for Core IR. Enforces Law 4
    (Idempotent Normalization): normalize (normalize p) == normalize p normalize_expr
    (normalize_expr e) == normalize_expr e Fulfills constant folding for basic
    arithmetic/boolean operations and dead code elimination for unreachable conditional
    branches, while preserving span provenance (Law 3). *)

val normalize_expr : Core_ir.expr -> Core_ir.expr
(** Simplifies and constant-folds a Core IR expression in a single idempotent step. *)

val normalize_decl : Core_ir.decl -> Core_ir.decl
(** Simplifies and normalizes a Core IR top-level declaration. *)

val normalize : Core_ir.program -> Core_ir.program
(** Simplifies and normalizes a complete Core IR program. *)
