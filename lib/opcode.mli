(** Instruction Set Architecture (ISA) opcodes for the Vesper bytecode virtual machine.
    Stack-based instructions operating on runtime values, lexical environments, and call
    frames. *)

type t =
  | Op_Const of int
  | Op_Add
  | Op_Sub
  | Op_Mul
  | Op_Div
  | Op_Mod
  | Op_Neg
  | Op_Not
  | Op_Eq
  | Op_Neq
  | Op_Lt
  | Op_Le
  | Op_Gt
  | Op_Ge
  | Op_GetLocal of int
  | Op_SetLocal of int
  | Op_GetGlobal of string
  | Op_DefGlobal of string
  | Op_SetGlobal of string
  | Op_Jump of int
  | Op_JumpIfFalse of int
  | Op_Call of int
  | Op_Return
  | Op_Halt
  | Op_Pop
  | Op_Unit
  | Op_Closure of int
  | Op_TieRec of string

val to_string : t -> string
(** Returns human-readable mnemonic representation of an opcode. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for opcodes. *)

val equal : t -> t -> bool
(** Structural equality check for opcodes. *)
