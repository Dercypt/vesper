(** Type algebra, substitutions, and type schemes for Hindley-Milner type inference.
    Guarantees pure functional representation and determinism (Law 5). *)

module IntMap : Map.S with type key = int
module IntSet : Set.S with type elt = int

type t =
  | TyInt
  | TyBool
  | TyString
  | TyUnit
  | TyArrow of t * t
  | TyVar of int  (** Pure types in the Vesper type system. *)

type subst = t IntMap.t
(** Substitution mapping type variable identifiers to types. *)

type scheme =
  | Forall of int list * t
      (** Polytype scheme universally quantifying over type variables. *)

val empty_subst : subst
(** The identity substitution mapping no variables. *)

val singleton_subst : int -> t -> subst
(** Constructs a substitution mapping a single variable identifier to a type. *)

val apply : subst -> t -> t
(** Applies a substitution to a type, replacing type variables with their mapped types. *)

val apply_scheme : subst -> scheme -> scheme
(** Applies a substitution to a type scheme, preserving bound variables. *)

val ftv : t -> IntSet.t
(** Computes the set of free type variables appearing in a type. *)

val ftv_scheme : scheme -> IntSet.t
(** Computes the set of free type variables appearing in a type scheme. *)

val compose_subst : subst -> subst -> subst
(** Composes two substitutions [s1] and [s2] such that
    [(compose_subst s1 s2) ty = apply s1 (apply s2 ty)]. *)

val equal : t -> t -> bool
(** Structural equality check for types. *)

val scheme_equal : scheme -> scheme -> bool
(** Equivalence check for type schemes modulo alpha-renaming of bound type variables. *)

val normalize_vars : t -> t
(** Canonically renumbers type variables starting from 0 in order of appearance. *)

val normalize_scheme : scheme -> scheme
(** Canonically renumbers bound type variables in a scheme starting from 0. *)

val to_string : t -> string
(** Formats a type as a human-readable string (e.g., "int -> bool"). *)

val pp : Format.formatter -> t -> unit
(** Pretty-prints a type to a formatter. *)

val scheme_to_string : scheme -> string
(** Formats a type scheme as a human-readable string. *)

val pp_scheme : Format.formatter -> scheme -> unit
(** Pretty-prints a type scheme to a formatter. *)
