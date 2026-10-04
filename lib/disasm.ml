open Bytecode

let disassemble_instruction fmt (chunk : Bytecode.chunk) offset =
  if offset >= Array.length chunk.code then offset
  else
    let op = chunk.code.(offset) in
    let span = Bytecode.get_span chunk offset in
    let span_str = Ast.span_to_string span in
    Format.fprintf fmt "%04d  %-20s" offset (Opcode.to_string op);
    (match op with
    | Opcode.Op_Const idx ->
        let v = Bytecode.get_constant chunk idx in
        Format.fprintf fmt " ; %s" (Value.to_string v)
    | Opcode.Op_Closure idx ->
        let child = Bytecode.get_child chunk idx in
        let param_str = match child.param with Some p -> p | None -> "_" in
        Format.fprintf fmt " ; (fun %s, %d instrs)" param_str (Array.length child.code)
    | _ -> ());
    Format.fprintf fmt "  (%s)\n" span_str;
    offset + 1

let rec disassemble_chunk fmt (chunk : Bytecode.chunk) ~name =
  Format.fprintf fmt "== %s ==\n" name;
  if Array.length chunk.constants > 0 then (
    Format.fprintf fmt "Constants (%d):\n" (Array.length chunk.constants);
    Array.iteri
      (fun i c -> Format.fprintf fmt "  [%d] %s\n" i (Value.to_string c))
      chunk.constants);
  if Array.length chunk.captures > 0 then (
    Format.fprintf fmt "Captures (%d):\n" (Array.length chunk.captures);
    Array.iteri
      (fun i (name, slot) -> Format.fprintf fmt "  [%d] %s (slot %d)\n" i name slot)
      chunk.captures);
  Format.fprintf fmt "Bytecode (%d instructions):\n" (Array.length chunk.code);
  let offset = ref 0 in
  while !offset < Array.length chunk.code do
    offset := disassemble_instruction fmt chunk !offset
  done;
  if Array.length chunk.children > 0 then (
    Format.fprintf fmt "Child Chunks (%d):\n" (Array.length chunk.children);
    Array.iteri
      (fun i child ->
        let child_name =
          match child.param with
          | Some p -> Printf.sprintf "%s.fun_%s_%d" name p i
          | None -> Printf.sprintf "%s.child_%d" name i
        in
        disassemble_chunk fmt child ~name:child_name)
      chunk.children)

let to_string ?(name = "<chunk>") chunk =
  Format.asprintf "%a" (fun fmt c -> disassemble_chunk fmt c ~name) chunk

let pp fmt chunk = disassemble_chunk fmt chunk ~name:"<chunk>"
