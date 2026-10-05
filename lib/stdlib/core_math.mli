(** Pure mathematical operations and checked arithmetic. Adheres to Law 1 (Total
    Diagnosability & Host Isolation): functions never raise uncaught host exceptions on
    invalid arguments. *)

val abs : int -> int
(** Computes the absolute value of an integer. Safely handles [min_int] without host
    crashes. *)

val add : int -> int -> int
(** Integer addition. *)

val sub : int -> int -> int
(** Integer subtraction. *)

val mul : int -> int -> int
(** Integer multiplication. *)

val div : int -> int -> (int, string) result
(** Checked integer division. Returns [Error "Division by zero"] if divisor is 0, or
    handles overflow safely. *)

val rem : int -> int -> (int, string) result
(** Checked integer remainder (modulo). Returns [Error "Division by zero"] if divisor is
    0. *)

val pow : int -> int -> (int, string) result
(** Integer exponentiation. Returns [Error "Negative exponent"] if exponent is negative.
*)

val min : int -> int -> int
(** Returns the minimum of two integers. *)

val max : int -> int -> int
(** Returns the maximum of two integers. *)

val clamp : min:int -> max:int -> int -> int
(** Clamps an integer to the range [[min, max]]. If [min > max], uses [min]. *)

val sign : int -> int
(** Returns [-1] for negative numbers, [0] for zero, and [1] for positive numbers. *)

val succ : int -> int
(** Returns the successor of an integer ([n + 1]). *)

val pred : int -> int
(** Returns the predecessor of an integer ([n - 1]). *)

val is_zero : int -> bool
(** Returns true if the integer is zero. *)

val is_positive : int -> bool
(** Returns true if the integer is strictly greater than zero. *)

val is_negative : int -> bool
(** Returns true if the integer is strictly less than zero. *)

val is_even : int -> bool
(** Returns true if the integer is even. *)

val is_odd : int -> bool
(** Returns true if the integer is odd. *)
