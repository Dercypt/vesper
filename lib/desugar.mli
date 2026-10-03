(** Desugaring pass transforming surface AST into explicit Core IR. Enforces Law 3 (Strict
    Source Provenance) by guaranteeing 100% of lowered nodes retain valid source spans
    derived directly from their surface syntax origins. *)

val lower_expr : Ast.expr -> (Core_ir.expr, Ast.error) result
(** Lowers an AST expression into Core IR, currying multi-argument functions and
    desugaring logical compound operators into explicit conditional branches. *)

val lower_decl : Ast.decl -> (Core_ir.decl, Ast.error) result
(** Lowers an AST top-level declaration into a Core IR declaration. *)

val lower : Ast.program -> (Core_ir.program, Ast.error) result
(** Lowers a complete AST compilation unit into a Core IR program. *)
