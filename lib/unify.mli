(** Syntactic unification and occurs check for Hindley-Milner type inference. Guarantees
    Law 1 (Host Isolation & Total Diagnosability) and Law 3 (Span Provenance). *)

val occurs_check : int -> Types.t -> bool
(** Performs the occurs check: tests whether type variable [var_id] occurs freely within
    type [ty]. Prevents the synthesis of infinite (recursive) types. *)

val unify : span:Ast.span -> Types.t -> Types.t -> (Types.subst, Ast.error) result
(** Computes the most general unifier (MGU) of two types [t1] and [t2] as a substitution.
    Returns an informative [Ast.error] with source span provenance if unification fails
    due to type mismatch or an occurs check violation. *)
