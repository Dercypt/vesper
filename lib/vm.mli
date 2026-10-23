(** High-performance stack-based virtual machine for Vesper bytecode execution. Enforces
    Law 1 (Total Diagnosability & Host Isolation) and Law 5 (Deterministic Evaluation &
    Sound Termination) with exact behavioral parity to the tree-walking evaluator. *)

val default_fuel : int
(** Standard execution fuel budget allocated for VM dispatch steps (1,000,000). *)

type value = Value.t
(** VM runtime values, operating natively on Value.t. *)

type env = Env.t
(** VM lexical environments, operating natively on Env.t. *)

type call_frame = { chunk : Bytecode.chunk; ip : int; base_sp : int; env : env }
(** Activation record for a function call on the VM call stack. *)

type vm_state = {
  chunk : Bytecode.chunk;
  ip : int;
  stack : value array;
  sp : int;
  call_stack : call_frame list;
}
(** VM execution state. *)

type vm_outcome = { value : Value.t; env : Env.t; steps : int }
(** Complete VM evaluation outcome containing returned value, accumulated environment, and
    reduction step count. *)

type vm_raw_outcome = vm_outcome = { value : value; env : env; steps : int }
(** Low-level VM evaluation outcome retaining native VM values and environments. *)

val value_to_eval_value : value -> Value.t
(** Converts a native VM value into a tree-walking evaluator value. *)

val value_of_eval_value : Value.t -> value
(** Converts a tree-walking evaluator value into a native VM value. *)

val env_to_eval_env : env -> Env.t
(** Converts a native VM environment into an evaluator environment. *)

val env_of_eval_env : Env.t -> env
(** Converts an evaluator environment into a native VM environment. *)

val run : ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Value.t, Diagnostic.t) result
(** Executes a bytecode chunk starting from initial environment and fuel budget, returning
    the final value or a structured diagnostic error. *)

val run_vm : ?fuel:int -> ?env:env -> Bytecode.chunk -> (value, Diagnostic.t) result
(** Executes a bytecode chunk and returns the final native VM value. *)

val run_outcome :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (vm_outcome, Diagnostic.t) result
(** Executes a bytecode chunk and returns the full outcome with environment and step
    statistics. *)

val run_vm_outcome :
  ?fuel:int -> ?env:env -> Bytecode.chunk -> (vm_raw_outcome, Diagnostic.t) result
(** Executes a bytecode chunk and returns the full native outcome with VM environment and
    step statistics. *)

val run_ast_error :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Value.t, Ast.error) result
(** Executes a bytecode chunk, returning [Ast.error] on failure for compatibility with the
    evaluator harness. *)

val run_outcome_ast_error :
  ?fuel:int -> ?env:Env.t -> Bytecode.chunk -> (Eval.eval_outcome, Ast.error) result
(** Executes a bytecode chunk with statistics, returning [Eval.eval_outcome] and
    [Ast.error]. *)
