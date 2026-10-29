type chunk = {
  code : Opcode.t array;
  constants : t array;
  spans : Ast.span array;
  children : chunk array;
  param : string option;
  body : Core_ir.expr option;
  captures : (string * int) array;
}

and t =
  | VInt of int
  | VBool of bool
  | VString of string
  | VClosure of { param : string; body : Core_ir.expr; env : env }
  | VBytecodeClosure of { chunk : chunk; env : env }
  | VNative of string * (t -> (t, Diagnostic.t) result)
  | VUnit
  | VConstruct of string * t list
  | VRecord of (string * t) list

and env = (string * t) list

let int n = VInt n
let bool b = VBool b
let string s = VString s
let unit = VUnit
let construct c args = VConstruct (c, args)
let record fields = VRecord fields
let is_construct c = function VConstruct (tag, _) -> String.equal c tag | _ -> false
let construct_args = function VConstruct (_, args) -> Some args | _ -> None

let record_field name = function
  | VRecord fields -> List.assoc_opt name fields
  | _ -> None

let closure ~param ~body ~env = VClosure { param; body; env }
let bytecode_closure ~chunk ~env = VBytecodeClosure { chunk; env }
let native name f = VNative (name, f)

let type_of = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VString _ -> "string"
  | VClosure _ | VBytecodeClosure _ | VNative _ -> "function"
  | VUnit -> "unit"
  | VConstruct (name, _) -> name
  | VRecord _ -> "record"

let rec pp fmt = function
  | VInt n -> Format.fprintf fmt "%d" n
  | VBool b -> Format.fprintf fmt "%b" b
  | VString s -> Format.fprintf fmt "%S" s
  | VClosure { param; _ } -> Format.fprintf fmt "<fun %s>" param
  | VBytecodeClosure { chunk; _ } ->
      let param = match chunk.param with Some p -> p | None -> "_" in
      Format.fprintf fmt "<fun %s>" param
  | VNative (name, _) -> Format.fprintf fmt "<native %s>" name
  | VUnit -> Format.fprintf fmt "()"
  | VConstruct (c, []) -> Format.fprintf fmt "%s" c
  | VConstruct (c, args) ->
      Format.fprintf fmt "%s(%a)" c
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp)
        args
  | VRecord fields ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
           (fun fmt (k, v) -> Format.fprintf fmt "%s = %a" k pp v))
        fields

let to_string v = Format.asprintf "%a" pp v

let rec equal_chunk c1 c2 =
  let code_eq =
    Array.length c1.code = Array.length c2.code
    && Array.for_all2 Opcode.equal c1.code c2.code
  in
  let const_eq =
    Array.length c1.constants = Array.length c2.constants
    && Array.for_all2 equal c1.constants c2.constants
  in
  let children_eq =
    Array.length c1.children = Array.length c2.children
    && Array.for_all2 equal_chunk c1.children c2.children
  in
  let param_eq = Option.equal String.equal c1.param c2.param in
  let body_eq = Option.equal Core_ir.expr_equal c1.body c2.body in
  let captures_eq =
    Array.length c1.captures = Array.length c2.captures
    && Array.for_all2 (fun (s1, _) (s2, _) -> String.equal s1 s2) c1.captures c2.captures
  in
  code_eq && const_eq && children_eq && param_eq && body_eq && captures_eq

and equal v1 v2 =
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
      | VNative (n1, _), VNative (n2, _) -> String.equal n1 n2
      | VUnit, VUnit -> true
      | VConstruct (c1, args1), VConstruct (c2, args2) ->
          String.equal c1 c2 && List.equal (eq_val visited) args1 args2
      | VRecord f1, VRecord f2 ->
          List.equal
            (fun (k1, val1) (k2, val2) -> String.equal k1 k2 && eq_val visited val1 val2)
            f1 f2
      | ( VClosure { param = p1; body = b1; env = e1 },
          VClosure { param = p2; body = b2; env = e2 } ) ->
          if String.equal p1 p2 && Core_ir.expr_equal b1 b2 then
            eq_env ((v1, v2) :: visited) e1 e2
          else false
      | ( VBytecodeClosure { chunk = c1; env = e1 },
          VBytecodeClosure { chunk = c2; env = e2 } ) ->
          if equal_chunk c1 c2 then eq_env ((v1, v2) :: visited) e1 e2 else false
      | ( VBytecodeClosure { chunk = c1; env = e1 },
          VClosure { param = p2; body = b2; env = e2 } )
      | ( VClosure { param = p2; body = b2; env = e2 },
          VBytecodeClosure { chunk = c1; env = e1 } ) -> (
          match (c1.param, c1.body) with
          | Some p1, Some b1 ->
              if String.equal p1 p2 && Core_ir.expr_equal b1 b2 then
                eq_env ((v1, v2) :: visited) e1 e2
              else false
          | _ -> false)
      | _ -> false
  and eq_env visited e1 e2 =
    List.equal
      (fun (k1, val1) (k2, val2) -> String.equal k1 k2 && eq_val visited val1 val2)
      e1 e2
  in
  eq_val [] v1 v2
