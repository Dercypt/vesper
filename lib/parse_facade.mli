val tokenize : file:string -> string -> (Tokens.token list, Ast.error) result
(** Scans source string into a list of lexical tokens. Catches all scanning exceptions and
    returns structured errors (Law 1). *)

val parse_string : file:string -> string -> (Ast.program, Ast.error) result
(** Parses an entire program source string into an AST. Catches lexer and parser
    exceptions, ensuring host isolation (Law 1). *)

val parse_expr : file:string -> string -> (Ast.expr, Ast.error) result
(** Parses a single expression source string into an AST. Catches lexer and parser
    exceptions, ensuring host isolation (Law 1). *)
