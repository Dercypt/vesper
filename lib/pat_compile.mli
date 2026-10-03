(** Decision Tree Compilation for Pattern Matching. Decomposes pattern matrices into
    deterministic decision trees, enables lowering to Core IR, and provides pure
    step-bounded evaluation (Law 3 & Law 5). *)

type path =
  | Here
  | Field of string * path
  | Arg of int * path
  | TupleElem of int * path
      (** Value navigation path identifying a subterm within a matched expression. *)

val path_to_string : path -> string

type action = {
  action_id : int;
  span : Ast.span;
  bindings : (string * path) list;
  guard : Core_ir.expr option;
  body : Core_ir.expr;
}
(** Match action representing matched variable bindings and resultant body expression. *)

type decision_tree =
  | Leaf of action
  | SwitchLit of {
      target : path;
      span : Ast.span;
      cases : (Ast.literal * decision_tree) list;
      default : decision_tree option;
    }
  | SwitchConstructor of {
      target : path;
      span : Ast.span;
      cases : (string * decision_tree) list;
      default : decision_tree option;
    }
  | SwitchBool of {
      target : path;
      span : Ast.span;
      true_branch : decision_tree;
      false_branch : decision_tree;
    }
  | Guard of {
      bindings : (string * path) list;
      condition : Core_ir.expr;
      then_tree : decision_tree;
      else_tree : decision_tree;
    }
  | Failure of { span : Ast.span }  (** Decision tree intermediate representation. *)

val equal_tree : decision_tree -> decision_tree -> bool
(** Compares two decision trees for structural equivalence. *)

val to_string : decision_tree -> string
val pp : Format.formatter -> decision_tree -> unit

val compile :
  sig_env:Pattern.signature_env ->
  span:Ast.span ->
  target_expr:Core_ir.expr ->
  Pattern.clause list ->
  (decision_tree, Ast.error) result
(** Compiles a list of match clauses into a decision tree using matrix decomposition. *)

val lower_to_core :
  span:Ast.span -> target_var:string -> decision_tree -> (Core_ir.expr, Ast.error) result
(** Lowers a decision tree containing literal and boolean switches into Core IR
    conditional expressions. *)

val eval_tree :
  ?fuel:int -> Env.t -> Value.t -> decision_tree -> (Value.t, Ast.error) result
(** Evaluates a compiled decision tree against a runtime value within a given lexical
    environment. *)

val eval_match :
  ?fuel:int ->
  sig_env:Pattern.signature_env ->
  span:Ast.span ->
  Env.t ->
  Value.t ->
  Pattern.clause list ->
  (Value.t, Ast.error) result
(** Validates exhaustiveness and evaluates a sequence of pattern clauses against a value.
*)
