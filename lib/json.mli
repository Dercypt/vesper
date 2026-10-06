(** Minimal, dependency-free JSON value model, parser, and printer used by the LSP server.
    Parsing is total: malformed input yields [Error], never a host exception (Law 1). *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

val parse : string -> (t, string) result
(** Parses a complete JSON document. Trailing non-whitespace input is rejected. *)

val to_string : t -> string
(** Serializes a JSON value compactly with deterministic field ordering (insertion order).
*)

val member : string -> t -> t option
(** [member key obj] looks up [key] in a JSON object. Returns [None] for missing keys or
    non-object values. *)

val to_string_opt : t -> string option
(** Extracts a string payload. *)

val to_int_opt : t -> int option
(** Extracts an integer payload. *)

val to_list_opt : t -> t list option
(** Extracts a list payload. *)
