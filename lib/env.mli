(** Immutable lexical environment for Vesper runtime evaluation. Provides purely
    functional variable bindings and lexical scoping. *)

type t = Value.env
(** Alias for the persistent lexical environment. *)

val empty : t
(** The empty environment containing no bindings. *)

val extend : string -> Value.t -> t -> t
(** Extends an environment with a new identifier binding. *)

val lookup : string -> t -> Value.t option
(** Searches for a binding in the environment from newest to oldest. *)

val shadow : string -> Value.t -> t -> t
(** Shadows an existing identifier with a new value in a nested scope. *)

val extend_rec : string -> string -> Core_ir.expr -> t -> t
(** Ties an immutable knot for a recursive closure, binding [name] to a closure whose
    captured environment refers back to itself without mutation. *)

val mem : string -> t -> bool
(** Returns true if the identifier is bound in the environment. *)

val to_list : t -> (string * Value.t) list
(** Converts environment bindings to an association list. *)

val of_list : (string * Value.t) list -> t
(** Constructs an environment from an association list of identifier-value pairs. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for lexical environments. *)

val to_string : t -> string
(** Formats an environment as a human-readable string. *)
