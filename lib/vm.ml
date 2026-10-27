let default_fuel = 1_000_000

type value = Value.t
type env = Env.t
type call_frame = { chunk : Bytecode.chunk; ip : int; base_sp : int; env : env }

type vm_state = {
  chunk : Bytecode.chunk;
  ip : int;
  stack : value array;
  sp : int;
  call_stack : call_frame list;
}

type vm_outcome = { value : Value.t; env : Env.t; steps : int }
type vm_raw_outcome = vm_outcome = { value : value; env : env; steps : int }

let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e
let type_of = Value.type_of
let equal = Value.equal
let value_to_eval_value (v : value) : Value.t = v
let value_of_eval_value (v : Value.t) : value = v
let env_to_eval_env (env : env) : Env.t = env
let env_of_eval_env (env : Env.t) : env = env

let run_vm_outcome ?(fuel = default_fuel) ?(env = []) (main_chunk : Bytecode.chunk) :
    (vm_raw_outcome, Diagnostic.t) result =
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

  let rec dispatch () : (vm_raw_outcome, Diagnostic.t) result =
    if !cur_fuel <= 0 then
      let span = Bytecode.get_span !cur_chunk !ip in
      Error
        (Diagnostic.error ~code:Error_code.E5002_fuel_exhausted ~span
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
          match List.assoc_opt name !cur_env with
          | Some v ->
              push v;
              incr ip;
              dispatch ()
          | None -> (
              match List.assoc_opt name !globals with
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
          globals := (name, v) :: !globals;
          cur_env := (name, v) :: !cur_env;
          incr ip;
          dispatch ()
      | Opcode.Op_SetGlobal name ->
          let* v = pop span in
          globals := (name, v) :: !globals;
          cur_env := (name, v) :: !cur_env;
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
                      (type_of other))))
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
                (Diagnostic.error ~code:Error_code.E5001_division_by_zero ~span
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
                (Diagnostic.error ~code:Error_code.E5001_division_by_zero ~span
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
          let* v = pop span in
          match v with
          | Value.VInt x ->
              push (Value.VInt (-x));
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: operator '-' expects integer, got %s"
                      (type_of other))))
      | Opcode.Op_Not -> (
          let* v = pop span in
          match v with
          | Value.VBool b ->
              push (Value.VBool (not b));
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: operator '!' expects boolean, got %s"
                      (type_of other))))
      | Opcode.Op_Eq ->
          let* b = pop span in
          let* a = pop span in
          push (Value.VBool (equal a b));
          incr ip;
          dispatch ()
      | Opcode.Op_Neq ->
          let* b = pop span in
          let* a = pop span in
          push (Value.VBool (not (equal a b)));
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
              (fun env (name, slot) -> (name, !stack.(base + slot)) :: env)
              !cur_env child.captures
          in
          let closure_val =
            Value.VBytecodeClosure { chunk = child; env = captured_env }
          in
          push closure_val;
          incr ip;
          dispatch ()
      | Opcode.Op_TieRec name ->
          let top = peek () in
          (match top with
          | Value.VBytecodeClosure { chunk; env = c_env } ->
              let rec rec_env =
                (name, Value.VBytecodeClosure { chunk; env = rec_env }) :: c_env
              in
              !stack.(!sp - 1) <- Value.VBytecodeClosure { chunk; env = rec_env }
          | Value.VClosure { param; body; env = c_env } ->
              let rec rec_env =
                (name, Value.VClosure { param; body; env = rec_env }) :: c_env
              in
              !stack.(!sp - 1) <- Value.VClosure { param; body; env = rec_env }
          | _ -> ());
          incr ip;
          dispatch ()
      | Opcode.Op_Call _arity -> (
          let* arg = pop span in
          let* fn_val = pop span in
          match fn_val with
          | Value.VBytecodeClosure { chunk = callee_chunk; env = c_env } ->
              let frame =
                { chunk = !cur_chunk; ip = !ip + 1; base_sp = !sp; env = !cur_env }
              in
              call_stack := frame :: !call_stack;
              cur_chunk := callee_chunk;
              ip := 0;
              cur_env := c_env;
              push arg;
              dispatch ()
          | Value.VClosure { param; body; env = c_env } -> (
              match Compiler.compile_fun ~param body with
              | Ok callee_chunk ->
                  let frame =
                    { chunk = !cur_chunk; ip = !ip + 1; base_sp = !sp; env = !cur_env }
                  in
                  call_stack := frame :: !call_stack;
                  cur_chunk := callee_chunk;
                  ip := 0;
                  cur_env := c_env;
                  push arg;
                  dispatch ()
              | Error err -> Error (Diagnostic.of_ast_error err))
          | Value.VNative (_name, f) ->
              let* res = f arg in
              push res;
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: expected a function, got %s"
                      (type_of other))))
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

let run_outcome ?(fuel = default_fuel) ?(env = Env.empty) (main_chunk : Bytecode.chunk) :
    (vm_outcome, Diagnostic.t) result =
  run_vm_outcome ~fuel ~env main_chunk

let run_vm ?fuel ?env chunk =
  match run_vm_outcome ?fuel ?env chunk with
  | Ok outcome -> Ok outcome.value
  | Error diag -> Error diag

let run ?(fuel = default_fuel) ?(env = Env.empty) chunk =
  match run_vm_outcome ~fuel ~env chunk with
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
