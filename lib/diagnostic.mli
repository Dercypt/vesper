(** Structured compiler diagnostics with source span provenance and ANSI terminal
    formatting. Fulfills Law 1 (Total Diagnosability) and Law 3 (Strict Source
    Provenance). *)

type severity = Error | Warning | Note

type t = {
  code : Error_code.t;
  severity : severity;
  span : Ast.span;
  message : string;
  hint : string option;
}

val make :
  code:Error_code.t -> severity:severity -> span:Ast.span -> ?hint:string -> string -> t
(** Smart constructor for a structured diagnostic. *)

val error : code:Error_code.t -> span:Ast.span -> ?hint:string -> string -> t
(** Creates an [Error] severity diagnostic. *)

val warning : code:Error_code.t -> span:Ast.span -> ?hint:string -> string -> t
(** Creates a [Warning] severity diagnostic. *)

val note : code:Error_code.t -> span:Ast.span -> ?hint:string -> string -> t
(** Creates a [Note] severity diagnostic. *)

val severity_to_string : severity -> string
(** String representation of severity level ("error", "warning", "note"). *)

val to_ast_error : t -> Ast.error
(** Downcasts a diagnostic to a simple [Ast.error] carrying span and message. *)

val of_ast_error : ?code:Error_code.t -> ?hint:string -> Ast.error -> t
(** Upcasts an [Ast.error] to a structured diagnostic, inferring error codes where
    applicable. *)

val extract_line : source:string -> line:int -> string option
(** Extracts a 1-indexed line from source text. Safe against out-of-bounds line
    coordinates. *)

val render_terminal : ?use_color:bool -> ?source:string -> t -> string
(** Formats a diagnostic for terminal output, including source code snippet, column caret
    pointers, and optional ANSI color highlights. *)

val pp : ?use_color:bool -> ?source:string -> Format.formatter -> t -> unit
(** Pretty-prints a diagnostic to a [Format.formatter]. *)
