(** Disassembler for Vesper bytecode chunks. Translates bytecode instructions, constants,
    and source spans into formatted human-readable disassembly text. *)

val disassemble_instruction : Format.formatter -> Bytecode.chunk -> int -> int
(** Disassembles a single instruction at byte offset, printing it to the formatter and
    returning the offset of the subsequent instruction. *)

val disassemble_chunk : Format.formatter -> Bytecode.chunk -> name:string -> unit
(** Disassembles an entire bytecode chunk including its constant pool, instructions, and
    nested child chunks. *)

val to_string : ?name:string -> Bytecode.chunk -> string
(** Formats disassembled chunk as a string. *)

val pp : Format.formatter -> Bytecode.chunk -> unit
(** Pretty-printer for disassembled chunks. *)
