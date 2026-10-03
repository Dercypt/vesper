(** Pure, deterministic tree-walking evaluator for Core IR expressions and programs.
    Guarantees Law 1 (Host Isolation & Total Diagnosability) and Law 5 (Deterministic
    Evaluation & Sound Termination). *)

val default_fuel : int
(** Standard execution fuel budget allocated for runtime evaluation steps (1,000,000). *)

type eval_result = { value : Value.t; steps : int }
(** Evaluation result containing the returned value and executed step count. *)

type eval_outcome = { value : Value.t; env : Env.t; steps : int }
(** Program evaluation outcome containing the final value, accumulated environment, and
    total reduction steps. *)

val eval_expr : ?fuel:int -> Env.t -> Core_ir.expr -> (Value.t, Ast.error) result
(** Evaluates a Core IR expression in a given lexical environment. Fails safely with a
    structured error if fuel is exhausted, an unbound identifier is encountered, a type
    mismatch occurs, or division by zero is attempted. *)

val eval_expr_with_stats :
  ?fuel:int -> Env.t -> Core_ir.expr -> (eval_result, Ast.error) result
(** Evaluates a Core IR expression and returns both the resulting value and the reduction
    step count. Useful for determinism profiling and Law 5 invariant verification. *)

val eval_decl : ?fuel:int -> Env.t -> Core_ir.decl -> (Value.t * Env.t, Ast.error) result
(** Evaluates a single top-level Core IR declaration, returning the resulting value and
    the extended lexical environment. *)

val eval_program :
  ?fuel:int -> ?env:Env.t -> Core_ir.program -> (Value.t, Ast.error) result
(** Evaluates a complete Core IR program module sequentially, returning the value of the
    final declaration (or [Value.VUnit] if empty). *)

val eval_program_with_stats :
  ?fuel:int -> ?env:Env.t -> Core_ir.program -> (eval_outcome, Ast.error) result
(** Evaluates a complete Core IR program module with total step accounting. *)
