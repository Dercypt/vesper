let default_fuel = 1_000_000

type call_frame = { chunk : Bytecode.chunk; ip : int; base_sp : int; env : Env.t }

type vm_state = {
  chunk : Bytecode.chunk;
  ip : int;
  stack : Value.t array;
  sp : int;
  call_stack : call_frame list;
}

type vm_outcome = { value : Value.t; env : Env.t; steps : int }

let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e
let fun_cache : (string * Core_ir.expr, Bytecode.chunk) Hashtbl.t = Hashtbl.create 64

let resolve_callee_chunk (cur_chunk : Bytecode.chunk) (call_stack : call_frame list)
    ~param (body : Core_ir.expr) : Bytecode.chunk =
  let rec find_in_children (children : Bytecode.chunk array) =
    let len = Array.length children in
    let rec loop i =
      if i >= len then None
      else
        let ch = children.(i) in
        match ch.body with
        | Some b when Core_ir.expr_equal b body -> Some ch
        | _ -> (
            match find_in_children ch.children with
            | Some ch' -> Some ch'
            | None -> loop (i + 1))
    in
    loop 0
  in
  match find_in_children cur_chunk.children with
  | Some ch -> ch
  | None -> (
      let rec search_frames = function
        | [] -> None
        | (frame : call_frame) :: rest -> (
            match find_in_children frame.chunk.children with
            | Some ch -> Some ch
            | None -> search_frames rest)
      in
      match search_frames call_stack with
      | Some ch -> ch
      | None -> (
          match Hashtbl.find_opt fun_cache (param, body) with
          | Some ch -> ch
          | None ->
              let ch = Compiler.compile_fun ~param body in
              Hashtbl.add fun_cache (param, body) ch;
              ch))

let run_outcome ?(fuel = default_fuel) ?(env = Env.empty) (main_chunk : Bytecode.chunk) :
    (vm_outcome, Diagnostic.t) result =
  let stack = ref (Array.make 1024 Value.unit) in
  let sp = ref 0 in
  let cur_chunk = ref main_chunk in
  let ip = ref 0 in
  let call_stack = ref [] in
  let cur_env = ref env in
  let globals = ref env in
  let cur_fuel = ref fuel in
  let steps = ref 0 in

  let ensure_capacity required =
    if required >= Array.length !stack then (
      let new_len = max (required + 1) (Array.length !stack * 2) in
      let new_stack = Array.make new_len Value.unit in
      Array.blit !stack 0 new_stack 0 !sp;
      stack := new_stack)
  in

  let push v =
    ensure_capacity (!sp + 1);
    !stack.(!sp) <- v;
    incr sp
  in

  let pop span =
    if !sp <= 0 then
      Error
        (Diagnostic.error ~code:Error_code.E0002_internal_error ~span "VM stack underflow")
    else (
      decr sp;
      Ok !stack.(!sp))
  in

  let peek () = if !sp <= 0 then Value.unit else !stack.(!sp - 1) in

  let rec dispatch () : (vm_outcome, Diagnostic.t) result =
    if !cur_fuel <= 0 then
      let span = Bytecode.get_span !cur_chunk !ip in
      Error
        (Diagnostic.error ~code:Error_code.E0002_internal_error ~span
           "Execution budget exhausted")
    else if !ip >= Array.length !cur_chunk.code then
      let result = if !sp > 0 then !stack.(!sp - 1) else Value.unit in
      Ok { value = result; env = !globals; steps = !steps }
    else
      let op = !cur_chunk.code.(!ip) in
      let span = Bytecode.get_span !cur_chunk !ip in
      decr cur_fuel;
      incr steps;
      match op with
      | Opcode.Op_Halt ->
          let result = if !sp > 0 then !stack.(!sp - 1) else Value.unit in
          Ok { value = result; env = !globals; steps = !steps }
      | Opcode.Op_Unit ->
          push Value.unit;
          incr ip;
          dispatch ()
      | Opcode.Op_Const idx ->
          let v = Bytecode.get_constant !cur_chunk idx in
          push v;
          incr ip;
          dispatch ()
      | Opcode.Op_Pop ->
          let* _ = pop span in
          incr ip;
          dispatch ()
      | Opcode.Op_GetLocal slot ->
          let base = match !call_stack with [] -> 0 | frame :: _ -> frame.base_sp in
          push !stack.(base + slot);
          incr ip;
          dispatch ()
      | Opcode.Op_SetLocal slot ->
          let* v = pop span in
          let base = match !call_stack with [] -> 0 | frame :: _ -> frame.base_sp in
          !stack.(base + slot) <- v;
          incr ip;
          dispatch ()
      | Opcode.Op_GetGlobal name -> (
          match Env.lookup name !cur_env with
          | Some v ->
              push v;
              incr ip;
              dispatch ()
          | None -> (
              match Env.lookup name !globals with
              | Some v ->
                  push v;
                  incr ip;
                  dispatch ()
              | None ->
                  Error
                    (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                       (Printf.sprintf "Unbound variable '%s'" name))))
      | Opcode.Op_DefGlobal name ->
          let v = peek () in
          globals := Env.extend name v !globals;
          cur_env := Env.extend name v !cur_env;
          incr ip;
          dispatch ()
      | Opcode.Op_SetGlobal name ->
          let* v = pop span in
          globals := Env.extend name v !globals;
          cur_env := Env.extend name v !cur_env;
          incr ip;
          dispatch ()
      | Opcode.Op_Jump target ->
          ip := target;
          dispatch ()
      | Opcode.Op_JumpIfFalse target -> (
          let* cond = pop span in
          match cond with
          | Value.VBool false ->
              ip := target;
              dispatch ()
          | Value.VBool true ->
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf
                      "Type error: conditional condition must be a boolean, got %s"
                      (Value.type_of other))))
      | Opcode.Op_Add -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VInt (x + y));
              incr ip;
              dispatch ()
          | Value.VString x, Value.VString y ->
              push (Value.VString (x ^ y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '+' expects integers or strings"))
      | Opcode.Op_Sub -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VInt (x - y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '-' expects integers"))
      | Opcode.Op_Mul -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VInt (x * y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '*' expects integers"))
      | Opcode.Op_Div -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt _, Value.VInt 0 ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Division by zero")
          | Value.VInt x, Value.VInt y ->
              push (Value.VInt (x / y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '/' expects integers"))
      | Opcode.Op_Mod -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt _, Value.VInt 0 ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Division by zero")
          | Value.VInt x, Value.VInt y ->
              push (Value.VInt (x mod y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '%' expects integers"))
      | Opcode.Op_Neg -> (
          let* a = pop span in
          match a with
          | Value.VInt x ->
              push (Value.VInt (-x));
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: operator '-' expects integer, got %s"
                      (Value.type_of other))))
      | Opcode.Op_Not -> (
          let* a = pop span in
          match a with
          | Value.VBool b ->
              push (Value.VBool (not b));
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: operator '!' expects boolean, got %s"
                      (Value.type_of other))))
      | Opcode.Op_Eq ->
          let* b = pop span in
          let* a = pop span in
          push (Value.VBool (Value.equal a b));
          incr ip;
          dispatch ()
      | Opcode.Op_Neq ->
          let* b = pop span in
          let* a = pop span in
          push (Value.VBool (not (Value.equal a b)));
          incr ip;
          dispatch ()
      | Opcode.Op_Lt -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VBool (x < y));
              incr ip;
              dispatch ()
          | Value.VString x, Value.VString y ->
              push (Value.VBool (String.compare x y < 0));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '<' expects integers or strings"))
      | Opcode.Op_Le -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VBool (x <= y));
              incr ip;
              dispatch ()
          | Value.VString x, Value.VString y ->
              push (Value.VBool (String.compare x y <= 0));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '<=' expects integers or strings"))
      | Opcode.Op_Gt -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VBool (x > y));
              incr ip;
              dispatch ()
          | Value.VString x, Value.VString y ->
              push (Value.VBool (String.compare x y > 0));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '>' expects integers or strings"))
      | Opcode.Op_Ge -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | Value.VInt x, Value.VInt y ->
              push (Value.VBool (x >= y));
              incr ip;
              dispatch ()
          | Value.VString x, Value.VString y ->
              push (Value.VBool (String.compare x y >= 0));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '>=' expects integers or strings"))
      | Opcode.Op_Closure child_idx ->
          let child = Bytecode.get_child !cur_chunk child_idx in
          let base = match !call_stack with [] -> 0 | frame :: _ -> frame.base_sp in
          let captured_env =
            Array.fold_left
              (fun env (name, slot) -> Env.extend name !stack.(base + slot) env)
              !cur_env child.captures
          in
          let param = match child.param with Some p -> p | None -> "_" in
          let body =
            match child.body with
            | Some b -> b
            | None -> Core_ir.make_expr ~span (Lit (Int 0))
          in
          let closure_val = Value.closure ~param ~body ~env:captured_env in
          push closure_val;
          incr ip;
          dispatch ()
      | Opcode.Op_TieRec name ->
          let top = peek () in
          (match top with
          | Value.VClosure { param; body; env = c_env } ->
              let rec_env = Env.extend_rec name param body c_env in
              !stack.(!sp - 1) <- Value.closure ~param ~body ~env:rec_env
          | _ -> ());
          incr ip;
          dispatch ()
      | Opcode.Op_Call _arity -> (
          let* arg = pop span in
          let* fn_val = pop span in
          match fn_val with
          | Value.VClosure { param; body; env = c_env } ->
              let callee_chunk =
                resolve_callee_chunk !cur_chunk !call_stack ~param body
              in
              let frame =
                { chunk = !cur_chunk; ip = !ip + 1; base_sp = !sp; env = !cur_env }
              in
              call_stack := frame :: !call_stack;
              cur_chunk := callee_chunk;
              ip := 0;
              cur_env := c_env;
              push arg;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: expected a function, got %s"
                      (Value.type_of other))))
      | Opcode.Op_Return -> (
          let* return_val = pop span in
          match !call_stack with
          | [] ->
              push return_val;
              let res = { value = return_val; env = !globals; steps = !steps } in
              Ok res
          | caller_frame :: remaining_frames ->
              call_stack := remaining_frames;
              cur_chunk := caller_frame.chunk;
              ip := caller_frame.ip;
              cur_env := caller_frame.env;
              sp := caller_frame.base_sp;
              push return_val;
              dispatch ())
  in
  try dispatch ()
  with exn ->
    let span = Bytecode.get_span !cur_chunk !ip in
    Error
      (Diagnostic.error ~code:Error_code.E0002_internal_error ~span
         (Printf.sprintf "Runtime failure: %s" (Printexc.to_string exn)))

let run ?fuel ?env chunk =
  match run_outcome ?fuel ?env chunk with
  | Ok outcome -> Ok outcome.value
  | Error diag -> Error diag

let run_ast_error ?fuel ?env chunk =
  match run ?fuel ?env chunk with
  | Ok v -> Ok v
  | Error diag -> Error (Diagnostic.to_ast_error diag)

let run_outcome_ast_error ?fuel ?env chunk =
  match run_outcome ?fuel ?env chunk with
  | Ok outcome ->
      Ok { Eval.value = outcome.value; env = outcome.env; steps = outcome.steps }
  | Error diag -> Error (Diagnostic.to_ast_error diag)
