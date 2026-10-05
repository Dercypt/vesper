(** Safe optional value monad and utilities. Adheres to Law 1 (Total Diagnosability & Host
    Isolation). *)

type 'a t = 'a option
(** Standard option type. *)

val some : 'a -> 'a option
(** Constructs [Some x]. *)

val none : 'a option
(** The [None] value. *)

val is_some : 'a option -> bool
(** Returns true if the option contains a value. *)

val is_none : 'a option -> bool
(** Returns true if the option is [None]. *)

val value : default:'a -> 'a option -> 'a
(** Extracts the value from [Some x], or returns [default] if [None]. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** Transforms the wrapped value if present. *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
(** Monadic bind for options. *)

val fold : none:'b -> some:('a -> 'b) -> 'a option -> 'b
(** Eliminates an option with handlers for each case. *)

val to_result : error:'e -> 'a option -> ('a, 'e) result
(** Converts an option into a result, providing an error if [None]. *)

val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
(** Compares two options for equality given an element equality predicate. *)
