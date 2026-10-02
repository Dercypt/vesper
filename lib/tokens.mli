type token = Parser.token
(** Lexical token type corresponding to Menhir parser tokens. *)

val to_string : token -> string
(** Returns a human-readable string representation of a token. *)
