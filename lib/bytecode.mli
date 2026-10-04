(** Bytecode chunk representation and inspection for the Vesper virtual machine. Chunks
    contain instruction buffers, constant pools, and debug span tables fulfilling Law 3
    (Strict Source Provenance). *)

type chunk = {
  code : Opcode.t array;
  constants : Value.t array;
  spans : Ast.span array;
  children : chunk array;
  param : string option;
  body : Core_ir.expr option;
  captures : (string * int) array;
}

type t = chunk

val create :
  code:Opcode.t array ->
  constants:Value.t array ->
  spans:Ast.span array ->
  ?children:chunk array ->
  ?param:string ->
  ?body:Core_ir.expr ->
  ?captures:(string * int) array ->
  unit ->
  chunk
(** Smart constructor for a bytecode chunk. *)

val empty : chunk
(** Empty chunk containing no instructions. *)

val length : chunk -> int
(** Number of bytecode instructions in chunk. *)

val get_opcode : chunk -> int -> Opcode.t
(** Retrieves opcode at instruction offset, returning [Op_Halt] if out of bounds. *)

val get_span : chunk -> int -> Ast.span
(** Retrieves source span at instruction offset, returning [Ast.dummy_span] if out of
    bounds. *)

val get_constant : chunk -> int -> Value.t
(** Retrieves constant at index. *)

val get_child : chunk -> int -> chunk
(** Retrieves child chunk prototype at index. *)

val equal : chunk -> chunk -> bool
(** Compares two bytecode chunks for structural equivalence. *)

val pp : Format.formatter -> chunk -> unit
(** Pretty-printer for bytecode chunks. *)

val to_string : chunk -> string
(** Formats chunk as human-readable string. *)
