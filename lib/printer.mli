val expr_to_string : Ast.expr -> string
(** Pretty-prints an AST expression into canonical Vesper syntax with minimal,
    precedence-aware parentheses. *)

val decl_to_string : Ast.decl -> string
(** Pretty-prints an AST declaration terminated by a semicolon. *)

val to_string : Ast.program -> string
(** Pretty-prints an entire Vesper program into canonical syntax. Enforces Law 2
    (Invertible Syntax). *)
