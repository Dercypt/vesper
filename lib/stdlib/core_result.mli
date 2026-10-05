(** Safe result monad and error propagation. Adheres to Law 1 (Total Diagnosability & Host
    Isolation). *)

type ('a, 'e) t = ('a, 'e) result
(** Standard result type representing either success ([Ok]) or failure ([Error]). *)

val ok : 'a -> ('a, 'e) result
(** Constructs [Ok x]. *)

val error : 'e -> ('a, 'e) result
(** Constructs [Error e]. *)

val is_ok : ('a, 'e) result -> bool
(** Returns true if the result is [Ok]. *)

val is_error : ('a, 'e) result -> bool
(** Returns true if the result is [Error]. *)

val value : default:'a -> ('a, 'e) result -> 'a
(** Extracts the value if [Ok], or returns [default] if [Error]. *)

val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
(** Transforms the success value. *)

val map_error : ('e1 -> 'e2) -> ('a, 'e1) result -> ('a, 'e2) result
(** Transforms the error value. *)

val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** Monadic bind for results. *)

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c
(** Eliminates a result with handlers for success and error. *)

val to_option : ('a, 'e) result -> 'a option
(** Converts a result to an option, discarding the error details. *)

val equal :
  ('a -> 'a -> bool) -> ('e -> 'e -> bool) -> ('a, 'e) result -> ('a, 'e) result -> bool
(** Compares two results for equality given equality predicates for success and error
    types. *)
