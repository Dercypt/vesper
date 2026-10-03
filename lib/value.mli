(** Runtime values for the Vesper tree-walking interpreter. Represents pure, immutable
    evaluation results fulfilling Law 5 (Deterministic Evaluation & Sound Termination). *)

type t =
  | VInt of int
  | VBool of bool
  | VString of string
  | VClosure of { param : string; body : Core_ir.expr; env : env }
  | VUnit

and env = (string * t) list
(** Runtime lexical environment mapping variable identifiers to values. *)

val int : int -> t
(** Constructs an integer runtime value. *)

val bool : bool -> t
(** Constructs a boolean runtime value. *)

val string : string -> t
(** Constructs a string runtime value. *)

val unit : t
(** The unit runtime value. *)

val closure : param:string -> body:Core_ir.expr -> env:env -> t
(** Constructs a closure value capturing its lexical environment. *)

val type_of : t -> string
(** Returns human-readable runtime type name of a value (e.g. "int", "bool", "function").
*)

val to_string : t -> string
(** Formats a runtime value as a human-readable string. *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer for runtime values. *)

val equal : t -> t -> bool
(** Compares two runtime values for equality. Primitives and units compare by structural
    value; closures compare by physical equality. *)
