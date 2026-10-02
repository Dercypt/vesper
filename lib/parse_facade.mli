(** Facade for lexical analysis and Menhir parsing with structured diagnostics. Enforces
    Law 1 (Total Diagnosability) and Law 3 (Strict Source Provenance). *)

val tokenize : file:string -> string -> (Tokens.token list, Diagnostic.t) result
(** Scans the input string into a list of lexical tokens. Returns a structured
    [Diagnostic.t] upon lexical errors without host exceptions (Law 1). *)

val parse_string : file:string -> string -> (Ast.program, Diagnostic.t) result
(** Parses a compilation unit string into an AST program. Leverages Menhir incremental
    error state inspection to provide detailed error diagnostics with source line span
    retention (Law 1, Law 2, Law 3). *)

val parse_expr : file:string -> string -> (Ast.expr, Diagnostic.t) result
(** Parses an isolated expression string into an AST expression node. *)
