(** Error codes for Vesper compiler diagnostics. Standardized codes ensure predictable
    machine and human readability. *)

type t =
  | E0001_io_error
  | E0002_internal_error
  | E1001_unexpected_token
  | E1002_unterminated_string
  | E1003_unclosed_comment
  | E1004_invalid_escape
  | E1005_unexpected_char
  | E1006_integer_overflow
  | E1007_invalid_span
  | E1008_syntax_error
  | E2001_type_error
  | E3001_non_exhaustive_match
  | E3002_redundant_pattern
  | E3003_pattern_type_error
  | E4001_cyclic_dependency
  | E4002_unbound_module
  | E4003_interface_mismatch
  | E4004_private_member_access

val to_code_string : t -> string
(** Returns the formal string code (e.g., "E1001"). *)

val to_description : t -> string
(** Returns a short descriptive summary of the error code category. *)

val pp : Format.formatter -> t -> unit
(** Pretty-prints the error code as its code string. *)

val to_string : t -> string
(** Formats the error code as its code string. *)

val equal : t -> t -> bool
(** Structural equality check for error codes. *)
