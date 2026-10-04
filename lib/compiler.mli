(** Compiler lowering Core IR expressions and programs into compact Bytecode chunks.
    Tracks lexical scopes, manages constant deduplication, and records source spans for
    every emitted instruction (Law 3). *)

val compile_expr : ?env:Env.t -> Core_ir.expr -> (Bytecode.chunk, Ast.error) result
(** Compiles a Core IR expression into a standalone bytecode chunk ending in [Op_Halt]. *)

val compile_decl : ?env:Env.t -> Core_ir.decl -> (Bytecode.chunk, Ast.error) result
(** Compiles a single Core IR declaration into a bytecode chunk. *)

val compile_program : ?env:Env.t -> Core_ir.program -> (Bytecode.chunk, Ast.error) result
(** Compiles an entire Core IR program into an executable bytecode chunk. *)

val compile_fun :
  ?captures:(string * int) array -> param:string -> Core_ir.expr -> Bytecode.chunk
(** Compiles a lambda function body into a function bytecode chunk ending in [Op_Return].
*)

val compile : Ast.program -> (Bytecode.chunk, Ast.error) result
(** Lowers a surface AST program to Core IR and compiles it into a bytecode chunk. *)
