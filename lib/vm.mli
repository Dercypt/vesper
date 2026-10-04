(** High-performance stack-based virtual machine for Vesper bytecode execution. Enforces
    Law 1 (Total Diagnosability & Host Isolation) and Law 5 (Deterministic Evaluation &
    Sound Termination) with exact behavioral parity to the tree-walking evaluator. *)

val default_fuel : int
(** Standard execution fuel budget allocated for VM dispatch steps (1,000,000). *)

type call_frame = { chunk : Bytecode.chunk; ip : int; base_sp : int; env : Env.t }
(** Activation record for a function call on the VM call stack. *)

type vm_state = {
  chunk : Bytecode.chunk;
  ip : int;
  stack : Value.t array;
  sp : int;
  call_stack : call_frame list;
}
(** VM execution state. *)

type vm_outcome = { value : Value.t; env : Env.t; steps : int }
(** Complete VM evaluation outcome containing returned value, accumulated environment, and
    reduction step count. *)

val run : ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Value.t, Diagnostic.t) result
(** Executes a bytecode chunk starting from initial environment and fuel budget, returning
    the final value or a structured diagnostic error. *)

val run_outcome :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (vm_outcome, Diagnostic.t) result
(** Executes a bytecode chunk and returns the full outcome with environment and step
    statistics. *)

val run_ast_error :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Value.t, Ast.error) result
(** Executes a bytecode chunk, returning [Ast.error] on failure for compatibility with the
    evaluator harness. *)

val run_outcome_ast_error :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Eval.eval_outcome, Ast.error) result
(** Executes a bytecode chunk with statistics, returning [Eval.eval_outcome] and
    [Ast.error]. *)
