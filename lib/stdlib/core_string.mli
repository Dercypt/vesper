(** Pure immutable string operations and conversions. Adheres to Law 1 (Total
    Diagnosability & Host Isolation): functions never raise uncaught host exceptions on
    invalid bounds or empty inputs. *)

val length : string -> int
(** Returns the length of the string in bytes. *)

val is_empty : string -> bool
(** Returns true if the string has length 0. *)

val concat : ?sep:string -> string list -> string
(** Concatenates a list of strings with an optional separator (defaulting to empty
    string). *)

val split : on:char -> string -> string list
(** Splits a string by a delimiter character into a list of substrings. *)

val split_lines : string -> string list
(** Splits a string into lines, recognizing both '\n' and "\r\n". *)

val trim : string -> string
(** Returns a copy of the string without leading and trailing whitespace. *)

val slice : string -> start:int -> len:int -> (string, string) result
(** Extracts a substring starting at byte offset [start] with length [len]. Returns
    [Error "Index out of bounds"] if [start < 0], [len < 0], or [start + len > length]. *)

val sub : string -> start:int -> len:int -> (string, string) result
(** Alias for [slice]. *)

val starts_with : prefix:string -> string -> bool
(** Returns true if the string begins with [prefix]. *)

val ends_with : suffix:string -> string -> bool
(** Returns true if the string ends with [suffix]. *)

val contains : string -> sub:string -> bool
(** Returns true if [sub] appears anywhere within the string. *)

val index_of : string -> sub:string -> int option
(** Returns the first byte offset where [sub] appears in the string, or [None] if not
    found. *)

val of_int : int -> string
(** Formats an integer as a decimal string. *)

val to_int : string -> int option
(** Parses an integer from a string, or returns [None] if invalid. *)

val of_bool : bool -> string
(** Converts a boolean to "true" or "false". *)

val to_bool : string -> bool option
(** Parses "true" or "false" into a boolean option. *)

val to_char_list : string -> char list
(** Converts a string into a list of characters. *)

val of_char_list : char list -> string
(** Converts a list of characters into a string. *)
