type t =
  | VInt of int
  | VBool of bool
  | VString of string
  | VClosure of { param : string; body : Core_ir.expr; env : env }
  | VUnit

and env = (string * t) list

let int n = VInt n
let bool b = VBool b
let string s = VString s
let unit = VUnit
let closure ~param ~body ~env = VClosure { param; body; env }

let type_of = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VString _ -> "string"
  | VClosure _ -> "function"
  | VUnit -> "unit"

let pp fmt = function
  | VInt n -> Format.fprintf fmt "%d" n
  | VBool b -> Format.fprintf fmt "%b" b
  | VString s -> Format.fprintf fmt "%S" s
  | VClosure { param; _ } -> Format.fprintf fmt "<fun %s>" param
  | VUnit -> Format.fprintf fmt "()"

let to_string v = Format.asprintf "%a" pp v

let equal v1 v2 =
  let rec eq_val visited v1 v2 =
    if v1 == v2 then true
    else if
      List.exists (fun (a, b) -> (a == v1 && b == v2) || (a == v2 && b == v1)) visited
    then true
    else
      match (v1, v2) with
      | VInt a, VInt b -> a = b
      | VBool a, VBool b -> a = b
      | VString a, VString b -> String.equal a b
      | VUnit, VUnit -> true
      | ( VClosure { param = p1; body = b1; env = e1 },
          VClosure { param = p2; body = b2; env = e2 } ) ->
          if String.equal p1 p2 && Core_ir.expr_equal b1 b2 then
            eq_env ((v1, v2) :: visited) e1 e2
          else false
      | _ -> false
  and eq_env visited e1 e2 =
    List.equal
      (fun (k1, val1) (k2, val2) -> String.equal k1 k2 && eq_val visited val1 val2)
      e1 e2
  in
  eq_val [] v1 v2
