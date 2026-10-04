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

let create ~code ~constants ~spans ?(children = [||]) ?param ?body ?(captures = [||]) () =
  { code; constants; spans; children; param; body; captures }

let empty =
  {
    code = [||];
    constants = [||];
    spans = [||];
    children = [||];
    param = None;
    body = None;
    captures = [||];
  }

let length c = Array.length c.code

let get_opcode c idx =
  if idx >= 0 && idx < Array.length c.code then c.code.(idx) else Opcode.Op_Halt

let get_span c idx =
  if idx >= 0 && idx < Array.length c.spans then c.spans.(idx) else Ast.dummy_span

let get_constant c idx =
  if idx >= 0 && idx < Array.length c.constants then c.constants.(idx) else Value.unit

let get_child c idx =
  if idx >= 0 && idx < Array.length c.children then c.children.(idx) else empty

let rec equal c1 c2 =
  let code_eq =
    Array.length c1.code = Array.length c2.code
    && Array.for_all2 Opcode.equal c1.code c2.code
  in
  let const_eq =
    Array.length c1.constants = Array.length c2.constants
    && Array.for_all2 Value.equal c1.constants c2.constants
  in
  let children_eq =
    Array.length c1.children = Array.length c2.children
    && Array.for_all2 equal c1.children c2.children
  in
  let param_eq = Option.equal String.equal c1.param c2.param in
  let body_eq = Option.equal Core_ir.expr_equal c1.body c2.body in
  let captures_eq =
    Array.length c1.captures = Array.length c2.captures
    && Array.for_all2
         (fun (s1, i1) (s2, i2) -> String.equal s1 s2 && i1 = i2)
         c1.captures c2.captures
  in
  code_eq && const_eq && children_eq && param_eq && body_eq && captures_eq

let pp fmt c =
  Format.fprintf fmt "<chunk: %d instrs, %d consts, %d children>" (Array.length c.code)
    (Array.length c.constants) (Array.length c.children)

let to_string c = Format.asprintf "%a" pp c
