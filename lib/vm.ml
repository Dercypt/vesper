let default_fuel = 1_000_000

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VClosure of { chunk : Bytecode.chunk; env : env }
  | VConstruct of string * value list
  | VRecord of (string * value) list

and env = (string * value) list

type call_frame = { chunk : Bytecode.chunk; ip : int; base_sp : int; env : env }

type vm_state = {
  chunk : Bytecode.chunk;
  ip : int;
  stack : value array;
  sp : int;
  call_stack : call_frame list;
}

type vm_outcome = { value : Value.t; env : Env.t; steps : int }
type vm_raw_outcome = { value : value; env : env; steps : int }

let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

let type_of = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VString _ -> "string"
  | VUnit -> "unit"
  | VClosure _ -> "function"
  | VConstruct (name, _) -> name
  | VRecord _ -> "record"

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
      | VConstruct (c1, args1), VConstruct (c2, args2) ->
          String.equal c1 c2 && List.equal (eq_val visited) args1 args2
      | VRecord f1, VRecord f2 ->
          List.equal
            (fun (k1, val1) (k2, val2) -> String.equal k1 k2 && eq_val visited val1 val2)
            f1 f2
      | VClosure { chunk = c1; env = e1 }, VClosure { chunk = c2; env = e2 } ->
          if Bytecode.equal c1 c2 then eq_env ((v1, v2) :: visited) e1 e2 else false
      | _ -> false
  and eq_env visited e1 e2 =
    List.equal
      (fun (k1, val1) (k2, val2) -> String.equal k1 k2 && eq_val visited val1 val2)
      e1 e2
  in
  eq_val [] v1 v2

let value_to_eval_value (top_v : value) : Value.t =
  let visited = ref [] in
  let rec conv_val = function
    | VInt n -> Value.VInt n
    | VBool b -> Value.VBool b
    | VString s -> Value.VString s
    | VUnit -> Value.VUnit
    | VConstruct (c, args) -> Value.VConstruct (c, List.map conv_val args)
    | VRecord fields -> Value.VRecord (List.map (fun (k, v) -> (k, conv_val v)) fields)
    | VClosure { chunk; env } as clos -> (
        match List.assq_opt clos !visited with
        | Some res -> res
        | None -> (
            let param = match chunk.param with Some p -> p | None -> "_" in
            let body =
              match chunk.body with
              | Some b -> b
              | None -> Core_ir.make_expr ~span:Ast.dummy_span (Lit (Int 0))
            in
            match env with
            | (rec_name, self_clos) :: outer when self_clos == clos ->
                let outer_env = conv_env outer in
                let rec rec_env =
                  (rec_name, Value.VClosure { param; body; env = rec_env }) :: outer_env
                in
                let res = Value.VClosure { param; body; env = rec_env } in
                visited := (clos, res) :: !visited;
                res
            | _ ->
                let outer_env = conv_env env in
                let res = Value.VClosure { param; body; env = outer_env } in
                visited := (clos, res) :: !visited;
                res))
  and conv_env env = List.map (fun (k, v) -> (k, conv_val v)) env in
  conv_val top_v

let env_to_eval_env (env : env) : Env.t =
  List.map (fun (k, v) -> (k, value_to_eval_value v)) env

let value_of_eval_value (top_v : Value.t) : value =
  let visited = ref [] in
  let rec conv_val = function
    | Value.VInt n -> VInt n
    | Value.VBool b -> VBool b
    | Value.VString s -> VString s
    | Value.VUnit -> VUnit
    | Value.VConstruct (c, args) -> VConstruct (c, List.map conv_val args)
    | Value.VRecord fields -> VRecord (List.map (fun (k, v) -> (k, conv_val v)) fields)
    | Value.VClosure { param; body; env } as clos -> (
        match List.assq_opt clos !visited with
        | Some res -> res
        | None -> (
            let chunk =
              match Compiler.compile_fun ~param body with
              | Ok c -> c
              | Error _ -> Bytecode.empty
            in
            match env with
            | (rec_name, self_clos) :: outer when self_clos == clos ->
                let outer_env = conv_env outer in
                let rec rec_env =
                  (rec_name, VClosure { chunk; env = rec_env }) :: outer_env
                in
                let res = VClosure { chunk; env = rec_env } in
                visited := (clos, res) :: !visited;
                res
            | _ ->
                let outer_env = conv_env env in
                let res = VClosure { chunk; env = outer_env } in
                visited := (clos, res) :: !visited;
                res))
  and conv_env env = List.map (fun (k, v) -> (k, conv_val v)) env in
  conv_val top_v

let env_of_eval_env (env : Env.t) : env =
  List.map (fun (k, v) -> (k, value_of_eval_value v)) env

let const_to_vm_value = function
  | Value.VInt n -> VInt n
  | Value.VBool b -> VBool b
  | Value.VString s -> VString s
  | Value.VUnit -> VUnit
  | other -> value_of_eval_value other

let run_vm_outcome ?(fuel = default_fuel) ?(env = []) (main_chunk : Bytecode.chunk) :
    (vm_raw_outcome, Diagnostic.t) result =
  let stack = ref (Array.make 1024 VUnit) in
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
      let new_stack = Array.make new_len VUnit in
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

  let peek () = if !sp <= 0 then VUnit else !stack.(!sp - 1) in

  let rec dispatch () : (vm_raw_outcome, Diagnostic.t) result =
    if !cur_fuel <= 0 then
      let span = Bytecode.get_span !cur_chunk !ip in
      Error
        (Diagnostic.error ~code:Error_code.E5002_fuel_exhausted ~span
           "Execution budget exhausted")
    else if !ip >= Array.length !cur_chunk.code then
      let result = if !sp > 0 then !stack.(!sp - 1) else VUnit in
      Ok { value = result; env = !globals; steps = !steps }
    else
      let op = !cur_chunk.code.(!ip) in
      let span = Bytecode.get_span !cur_chunk !ip in
      decr cur_fuel;
      incr steps;
      match op with
      | Opcode.Op_Halt ->
          let result = if !sp > 0 then !stack.(!sp - 1) else VUnit in
          Ok { value = result; env = !globals; steps = !steps }
      | Opcode.Op_Unit ->
          push VUnit;
          incr ip;
          dispatch ()
      | Opcode.Op_Const idx ->
          let v = const_to_vm_value (Bytecode.get_constant !cur_chunk idx) in
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
          | VBool false ->
              ip := target;
              dispatch ()
          | VBool true ->
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
          | VInt x, VInt y ->
              push (VInt (x + y));
              incr ip;
              dispatch ()
          | VString x, VString y ->
              push (VString (x ^ y));
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
          | VInt x, VInt y ->
              push (VInt (x - y));
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
          | VInt x, VInt y ->
              push (VInt (x * y));
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
          | VInt _, VInt 0 ->
              Error
                (Diagnostic.error ~code:Error_code.E5001_division_by_zero ~span
                   "Division by zero")
          | VInt x, VInt y ->
              push (VInt (x / y));
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
          | VInt _, VInt 0 ->
              Error
                (Diagnostic.error ~code:Error_code.E5001_division_by_zero ~span
                   "Division by zero")
          | VInt x, VInt y ->
              push (VInt (x mod y));
              incr ip;
              dispatch ()
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   "Type error: operator '%' expects integers"))
      | Opcode.Op_Neg -> (
          let* a = pop span in
          match a with
          | VInt x ->
              push (VInt (-x));
              incr ip;
              dispatch ()
          | other ->
              Error
                (Diagnostic.error ~code:Error_code.E2001_type_error ~span
                   (Printf.sprintf "Type error: operator '-' expects integer, got %s"
                      (type_of other))))
      | Opcode.Op_Not -> (
          let* a = pop span in
          match a with
          | VBool b ->
              push (VBool (not b));
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
          push (VBool (equal a b));
          incr ip;
          dispatch ()
      | Opcode.Op_Neq ->
          let* b = pop span in
          let* a = pop span in
          push (VBool (not (equal a b)));
          incr ip;
          dispatch ()
      | Opcode.Op_Lt -> (
          let* b = pop span in
          let* a = pop span in
          match (a, b) with
          | VInt x, VInt y ->
              push (VBool (x < y));
              incr ip;
              dispatch ()
          | VString x, VString y ->
              push (VBool (String.compare x y < 0));
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
          | VInt x, VInt y ->
              push (VBool (x <= y));
              incr ip;
              dispatch ()
          | VString x, VString y ->
              push (VBool (String.compare x y <= 0));
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
          | VInt x, VInt y ->
              push (VBool (x > y));
              incr ip;
              dispatch ()
          | VString x, VString y ->
              push (VBool (String.compare x y > 0));
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
          | VInt x, VInt y ->
              push (VBool (x >= y));
              incr ip;
              dispatch ()
          | VString x, VString y ->
              push (VBool (String.compare x y >= 0));
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
          let closure_val = VClosure { chunk = child; env = captured_env } in
          push closure_val;
          incr ip;
          dispatch ()
      | Opcode.Op_TieRec name ->
          let top = peek () in
          (match top with
          | VClosure { chunk; env = c_env } ->
              let rec rec_env = (name, VClosure { chunk; env = rec_env }) :: c_env in
              !stack.(!sp - 1) <- VClosure { chunk; env = rec_env }
          | _ -> ());
          incr ip;
          dispatch ()
      | Opcode.Op_Call _arity -> (
          let* arg = pop span in
          let* fn_val = pop span in
          match fn_val with
          | VClosure { chunk = callee_chunk; env = c_env } ->
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
  let vm_env = env_of_eval_env env in
  match run_vm_outcome ~fuel ~env:vm_env main_chunk with
  | Ok raw ->
      Ok
        {
          value = value_to_eval_value raw.value;
          env = env_to_eval_env raw.env;
          steps = raw.steps;
        }
  | Error diag -> Error diag

let run_vm ?fuel ?env chunk =
  match run_vm_outcome ?fuel ?env chunk with
  | Ok outcome -> Ok outcome.value
  | Error diag -> Error diag

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
