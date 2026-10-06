(** Interactive Read-Eval-Print Loop. Maintains persistent typing and evaluation
    environments across inputs and never terminates on user errors (Law 1). *)

type t
(** Persistent REPL session state (typing environment and runtime environment). *)

val initial : unit -> t
(** Fresh session seeded with the standard prelude. *)

val eval_input : t -> string -> (t * string, Diagnostic.t list) result
(** [eval_input st src] parses, typechecks, and evaluates the declarations in [src].
    Returns the updated session and the rendered result lines (e.g. ["val x : int = 42"]).
    On any error the original session is left untouched by the caller (state is only
    advanced on success). *)

val run :
  read_line:(unit -> string option) -> out:(string -> unit) -> err:(string -> unit) -> int
(** Drives a multi-line session. Input accumulates until a line ends with [;]. Syntax,
    type, and runtime errors are rendered to [err] and the loop continues. [:quit] / [:q]
    or end of input terminate the session. Returns the process exit code (always 0). *)
