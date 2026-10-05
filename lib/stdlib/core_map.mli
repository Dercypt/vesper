(** Pure immutable key-value map with string keys. Adheres to Law 1 (Total Diagnosability
    & Host Isolation) and Law 5 (Determinism). *)

type 'a t
(** Immutable map type indexed by string keys. *)

val empty : 'a t
(** An empty map. *)

val is_empty : 'a t -> bool
(** Returns true if the map contains no entries. *)

val add : string -> 'a -> 'a t -> 'a t
(** Associates a key with a value, replacing any existing binding. *)

val find_opt : string -> 'a t -> 'a option
(** Looks up a key, returning [Some v] if present or [None] otherwise. *)

val find : string -> 'a t -> ('a, string) result
(** Looks up a key, returning [Ok v] if present or [Error "Key not found: <k>"] otherwise.
*)

val remove : string -> 'a t -> 'a t
(** Removes a key from the map. Returns unchanged map if key is not present. *)

val mem : string -> 'a t -> bool
(** Returns true if the key is bound in the map. *)

val cardinal : 'a t -> int
(** Returns the total number of bindings in the map. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Transforms values associated with all keys. *)

val mapi : (string -> 'a -> 'b) -> 'a t -> 'b t
(** Transforms values with key context. *)

val fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** Folds over all key-value bindings in key order. *)

val iter : (string -> 'a -> unit) -> 'a t -> unit
(** Iterates over all key-value bindings. *)

val to_list : 'a t -> (string * 'a) list
(** Returns all bindings as a sorted association list. *)

val of_list : (string * 'a) list -> 'a t
(** Constructs a map from an association list of key-value pairs. *)

val keys : 'a t -> string list
(** Returns all keys in the map in sorted order. *)

val values : 'a t -> 'a list
(** Returns all values in the map ordered by their keys. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Compares two maps for equality using an element equality predicate. *)
