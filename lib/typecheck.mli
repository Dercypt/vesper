(** Static analysis, bidirectional type checking, and Hindley-Milner type inference.
    Guarantees Law 1 (Total Diagnosability & Host Isolation) and Law 3 (Strict Source
    Provenance). *)

type typed_expr = { span : Ast.span; desc : Ast.expr_desc; ty : Types.t }
(** Typed expression node preserving surface syntax provenance and inferred type. *)

type typed_decl_desc =
  | TypedLetDecl of {
      name : string;
      is_rec : bool;
      args : string list;
      value : typed_expr;
      scheme : Types.scheme;
    }
  | TypedExprDecl of typed_expr

type typed_decl = { span : Ast.span; desc : typed_decl_desc }
(** Typed declaration node carrying inferred type scheme and source provenance. *)

type typed_program = { span : Ast.span; decls : typed_decl list; env : Type_env.t }
(** Typed compilation unit containing typed declarations and resulting typing environment.
*)

type typed_ir = typed_program
(** Typed IR representation produced by the typechecking pass. *)

val infer : Type_env.t -> Ast.expr -> (Types.subst * Types.t, Ast.error) result
(** Synthesizes (infers) the most general type of an AST expression under a given typing
    environment using Hindley-Milner Algorithm W. Returns the inferred substitution and
    type. *)

val check : Type_env.t -> Ast.expr -> Types.t -> (Types.subst, Ast.error) result
(** Verifies (checks) that an AST expression conforms to an expected type in bidirectional
    type checking mode. *)

val typecheck_expr : ?env:Type_env.t -> Ast.expr -> (typed_expr, Ast.error) result
(** Typechecks an expression, returning a [typed_expr] with all substitutions applied to
    its type. *)

val typecheck_decl :
  ?env:Type_env.t -> Ast.decl -> (typed_decl * Type_env.t, Ast.error) result
(** Typechecks a top-level declaration, returning the typed declaration and the extended
    typing environment. *)

val typecheck_program :
  ?env:Type_env.t -> Ast.program -> (typed_program, Diagnostic.t list) result
(** Pure typechecking pass over a complete AST compilation unit. Returns the typed program
    or structured compiler diagnostics. *)

val typecheck : Ast.program -> (typed_program, Diagnostic.t list) result
(** Alias for [typecheck_program]. *)
