(** Maranget's Matrix-Based Pattern Exhaustiveness and Redundancy Checker. Guarantees
    match safety, synthesizes counterexample witnesses for uncovered patterns, and reports
    dead-code warnings with source spans (Law 1 & Law 3). *)

type matrix = Pattern.pattern list list
(** Pattern matrix where each row represents a clause vector. *)

type check_result = {
  is_exhaustive : bool;
  missing_witness : Pattern.pattern option;
  redundant_indices : int list;
  diagnostics : Diagnostic.t list;
}
(** Complete analysis result containing exhaustiveness status, counterexample witness,
    indices of unreachable clauses, and structured diagnostics. *)

val is_useful :
  sig_env:Pattern.signature_env -> matrix:matrix -> vector:Pattern.pattern list -> bool
(** Decides whether a pattern vector is useful (matches at least one value not matched by
    previous rows) with respect to a pattern matrix according to Maranget's algorithm. *)

val find_witness :
  sig_env:Pattern.signature_env ->
  span:Ast.span ->
  matrix:matrix ->
  ?expected_type:string ->
  unit ->
  Pattern.pattern option
(** Synthesizes an unmatched witness pattern if the matrix is non-exhaustive. *)

val check_patterns :
  sig_env:Pattern.signature_env ->
  span:Ast.span ->
  ?expected_type:string ->
  Pattern.pattern list ->
  check_result
(** Analyzes a sequence of patterns for exhaustiveness and redundancy. *)

val check_clauses :
  sig_env:Pattern.signature_env ->
  span:Ast.span ->
  ?expected_type:string ->
  Pattern.clause list ->
  check_result
(** Analyzes a list of match clauses for exhaustiveness and redundancy. *)
