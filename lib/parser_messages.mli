(** Syntax error message catalog for Menhir parser states. *)

val message : int -> string
(** [message state] returns a human-readable explanation of the syntax error associated
    with LR(1) state [state]. Raises [Not_found] if the state is not recognized in the
    catalog. *)
