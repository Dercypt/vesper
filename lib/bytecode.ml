type chunk = Value.chunk = {
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

let equal = Value.equal_chunk

let pp fmt c =
  Format.fprintf fmt "<chunk: %d instrs, %d consts, %d children>" (Array.length c.code)
    (Array.length c.constants) (Array.length c.children)

let to_string c = Format.asprintf "%a" pp c
