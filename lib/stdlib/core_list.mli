(** Pure functional list operations. Adheres to Law 1 (Total Diagnosability & Host
    Isolation): operations are total and safe against empty-list crashes. *)

val length : 'a list -> int
(** Returns the number of elements in the list. *)

val is_empty : 'a list -> bool
(** Returns true if the list is empty. *)

val head : 'a list -> 'a option
(** Returns the first element of the list, or [None] if empty. *)

val tail : 'a list -> 'a list option
(** Returns all elements except the first, or [None] if empty. *)

val cons : 'a -> 'a list -> 'a list
(** Prepends an element to the list. *)

val map : ('a -> 'b) -> 'a list -> 'b list
(** Applies a transformation function to each element. *)

val filter : ('a -> bool) -> 'a list -> 'a list
(** Keeps only the elements satisfying a predicate. *)

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc
(** Left-associative fold over elements. Tail-recursive. *)

val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc
(** Right-associative fold over elements. *)

val reverse : 'a list -> 'a list
(** Reverses the elements of a list. *)

val append : 'a list -> 'a list -> 'a list
(** Concatenates two lists. *)

val concat : 'a list list -> 'a list
(** Flattens a list of lists into a single list. *)

val zip : 'a list -> 'b list -> ('a * 'b) list
(** Combines two lists pairwise. Stops at the length of the shorter list. *)

val zip_with : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** Combines two lists using a pairwise combiner function. *)

val take : int -> 'a list -> 'a list
(** Returns the first [n] elements of the list (or all if [n >= length]). *)

val drop : int -> 'a list -> 'a list
(** Drops the first [n] elements from the list. *)

val nth : int -> 'a list -> 'a option
(** Returns the element at 0-indexed position [n], or [None] if out of bounds. *)

val find : ('a -> bool) -> 'a list -> 'a option
(** Returns the first element satisfying the predicate, or [None]. *)

val exists : ('a -> bool) -> 'a list -> bool
(** Returns true if at least one element satisfies the predicate. *)

val for_all : ('a -> bool) -> 'a list -> bool
(** Returns true if all elements satisfy the predicate. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
(** Splits a list into elements that satisfy the predicate and elements that do not. *)

val flatten : 'a list list -> 'a list
(** Alias for [concat]. *)

val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
(** Compares two lists for equality given an element equality function. *)
