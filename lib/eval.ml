let default_fuel = 1_000_000

type eval_result = { value : Value.t; steps : int }
type eval_outcome = { value : Value.t; env : Env.t; steps : int }

let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e
let err span message : ('a, Ast.error) result = Error { Ast.span; message }

let eval_primop ~span (op : Core_ir.primop) args steps fuel =
  match (op, args) with
  | Core_ir.Neg, [ Value.VInt n ] -> Ok (Value.VInt (-n), steps, fuel)
  | Core_ir.Neg, [ other ] ->
      err span
        (Printf.sprintf "Type error: operator '-' expects integer, got %s"
           (Value.type_of other))
  | Core_ir.Neg, _ -> err span "Operator '-' expects 1 argument"
  | Core_ir.Not, [ Value.VBool b ] -> Ok (Value.VBool (not b), steps, fuel)
  | Core_ir.Not, [ other ] ->
      err span
        (Printf.sprintf "Type error: operator '!' expects boolean, got %s"
           (Value.type_of other))
  | Core_ir.Not, _ -> err span "Operator '!' expects 1 argument"
  | Core_ir.Add, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VInt (a + b), steps, fuel)
  | Core_ir.Add, [ Value.VString a; Value.VString b ] ->
      Ok (Value.VString (a ^ b), steps, fuel)
  | Core_ir.Add, [ _; _ ] ->
      err span "Type error: operator '+' expects integers or strings"
  | Core_ir.Add, _ -> err span "Operator '+' expects 2 arguments"
  | Core_ir.Sub, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VInt (a - b), steps, fuel)
  | Core_ir.Sub, [ _; _ ] -> err span "Type error: operator '-' expects integers"
  | Core_ir.Sub, _ -> err span "Operator '-' expects 2 arguments"
  | Core_ir.Mul, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VInt (a * b), steps, fuel)
  | Core_ir.Mul, [ _; _ ] -> err span "Type error: operator '*' expects integers"
  | Core_ir.Mul, _ -> err span "Operator '*' expects 2 arguments"
  | Core_ir.Div, [ Value.VInt _; Value.VInt 0 ] -> err span "Division by zero"
  | Core_ir.Div, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VInt (a / b), steps, fuel)
  | Core_ir.Div, [ _; _ ] -> err span "Type error: operator '/' expects integers"
  | Core_ir.Div, _ -> err span "Operator '/' expects 2 arguments"
  | Core_ir.Mod, [ Value.VInt _; Value.VInt 0 ] -> err span "Division by zero"
  | Core_ir.Mod, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VInt (a mod b), steps, fuel)
  | Core_ir.Mod, [ _; _ ] -> err span "Type error: operator '%' expects integers"
  | Core_ir.Mod, _ -> err span "Operator '%' expects 2 arguments"
  | Core_ir.Eq, [ v1; v2 ] -> Ok (Value.VBool (Value.equal v1 v2), steps, fuel)
  | Core_ir.Eq, _ -> err span "Operator '==' expects 2 arguments"
  | Core_ir.Neq, [ v1; v2 ] -> Ok (Value.VBool (not (Value.equal v1 v2)), steps, fuel)
  | Core_ir.Neq, _ -> err span "Operator '!=' expects 2 arguments"
  | Core_ir.Lt, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VBool (a < b), steps, fuel)
  | Core_ir.Lt, [ Value.VString a; Value.VString b ] ->
      Ok (Value.VBool (String.compare a b < 0), steps, fuel)
  | Core_ir.Lt, [ _; _ ] ->
      err span "Type error: operator '<' expects integers or strings"
  | Core_ir.Lt, _ -> err span "Operator '<' expects 2 arguments"
  | Core_ir.Le, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VBool (a <= b), steps, fuel)
  | Core_ir.Le, [ Value.VString a; Value.VString b ] ->
      Ok (Value.VBool (String.compare a b <= 0), steps, fuel)
  | Core_ir.Le, [ _; _ ] ->
      err span "Type error: operator '<=' expects integers or strings"
  | Core_ir.Le, _ -> err span "Operator '<=' expects 2 arguments"
  | Core_ir.Gt, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VBool (a > b), steps, fuel)
  | Core_ir.Gt, [ Value.VString a; Value.VString b ] ->
      Ok (Value.VBool (String.compare a b > 0), steps, fuel)
  | Core_ir.Gt, [ _; _ ] ->
      err span "Type error: operator '>' expects integers or strings"
  | Core_ir.Gt, _ -> err span "Operator '>' expects 2 arguments"
  | Core_ir.Ge, [ Value.VInt a; Value.VInt b ] -> Ok (Value.VBool (a >= b), steps, fuel)
  | Core_ir.Ge, [ Value.VString a; Value.VString b ] ->
      Ok (Value.VBool (String.compare a b >= 0), steps, fuel)
  | Core_ir.Ge, [ _; _ ] ->
      err span "Type error: operator '>=' expects integers or strings"
  | Core_ir.Ge, _ -> err span "Operator '>=' expects 2 arguments"

let rec eval_step (env : Env.t) (fuel : int) (steps : int) (e : Core_ir.expr) :
    (Value.t * int * int, Ast.error) result =
  if fuel <= 0 then err e.span "Execution budget exhausted"
  else
    match e.desc with
    | Lit (Int n) -> Ok (Value.VInt n, steps + 1, fuel)
    | Lit (Bool b) -> Ok (Value.VBool b, steps + 1, fuel)
    | Lit (String s) -> Ok (Value.VString s, steps + 1, fuel)
    | Var id -> (
        match Env.lookup id env with
        | Some v -> Ok (v, steps + 1, fuel)
        | None -> err e.span (Printf.sprintf "Unbound variable '%s'" id))
    | Fun { param; body } -> Ok (Value.VClosure { param; body; env }, steps + 1, fuel)
    | Let { name; is_rec; value; body } ->
        if is_rec then
          match value.desc with
          | Fun { param; body = fn_body } ->
              let rec_env = Env.extend_rec name param fn_body env in
              eval_step rec_env fuel (steps + 1) body
          | _ -> (
              let* v, steps', fuel' = eval_step env fuel steps value in
              match v with
              | Value.VClosure { param; body = fn_body; env = c_env } ->
                  let rec_env = Env.extend_rec name param fn_body c_env in
                  let body_env =
                    Env.extend name
                      (Value.VClosure { param; body = fn_body; env = rec_env })
                      env
                  in
                  eval_step body_env fuel' steps' body
              | non_closure ->
                  let body_env = Env.extend name non_closure env in
                  eval_step body_env fuel' steps' body)
        else
          let* v, steps', fuel' = eval_step env fuel steps value in
          let body_env = Env.extend name v env in
          eval_step body_env fuel' steps' body
    | App { fn; arg } -> (
        let* fn_val, steps_fn, fuel_fn = eval_step env fuel steps fn in
        let* arg_val, steps_arg, fuel_arg = eval_step env fuel_fn steps_fn arg in
        if fuel_arg <= 1 then err e.span "Execution budget exhausted"
        else
          match fn_val with
          | Value.VClosure { param; body; env = c_env } ->
              let call_env = Env.extend param arg_val c_env in
              eval_step call_env (fuel_arg - 1) (steps_arg + 1) body
          | other ->
              err fn.span
                (Printf.sprintf "Type error: expected a function, got %s"
                   (Value.type_of other)))
    | If { cond; then_branch; else_branch } -> (
        let* cond_val, steps_c, fuel_c = eval_step env fuel steps cond in
        match cond_val with
        | Value.VBool true -> eval_step env fuel_c (steps_c + 1) then_branch
        | Value.VBool false -> eval_step env fuel_c (steps_c + 1) else_branch
        | other ->
            err cond.span
              (Printf.sprintf
                 "Type error: conditional condition must be a boolean, got %s"
                 (Value.type_of other)))
    | PrimOp { op; args } ->
        let rec eval_args cur_env cur_fuel cur_steps acc = function
          | [] -> eval_primop ~span:e.span op (List.rev acc) cur_steps cur_fuel
          | a :: rest ->
              let* v, s', f' = eval_step cur_env cur_fuel cur_steps a in
              eval_args cur_env f' s' (v :: acc) rest
        in
        eval_args env fuel steps [] args

let eval_decl ?(fuel = default_fuel) (env : Env.t) (d : Core_ir.decl) :
    (Value.t * Env.t, Ast.error) result =
  try
    if fuel <= 0 then err d.span "Execution budget exhausted"
    else
      match d.desc with
      | Core_ir.LetDecl { name; is_rec; value } ->
          if is_rec then
            match value.desc with
            | Fun { param; body } ->
                let rec_env = Env.extend_rec name param body env in
                let closure_val = Value.closure ~param ~body ~env:rec_env in
                Ok (closure_val, rec_env)
            | _ -> (
                let* v, _, _ = eval_step env fuel 0 value in
                match v with
                | Value.VClosure { param; body; env = c_env } ->
                    let rec_env = Env.extend_rec name param body c_env in
                    let closure_val = Value.closure ~param ~body ~env:rec_env in
                    Ok (closure_val, Env.extend name closure_val env)
                | non_closure -> Ok (non_closure, Env.extend name non_closure env))
          else
            let* v, _, _ = eval_step env fuel 0 value in
            Ok (v, Env.extend name v env)
      | Core_ir.ExprDecl e ->
          let* v, _, _ = eval_step env fuel 0 e in
          Ok (v, env)
  with exn -> err d.span (Printf.sprintf "Runtime failure: %s" (Printexc.to_string exn))

let eval_program_with_stats ?(fuel = default_fuel) ?(env = Env.empty)
    (p : Core_ir.program) : (eval_outcome, Ast.error) result =
  try
    let rec loop cur_env cur_fuel cur_steps last_val = function
      | [] -> Ok { value = last_val; env = cur_env; steps = cur_steps }
      | (d : Core_ir.decl) :: rest -> (
          if cur_fuel <= 0 then err d.span "Execution budget exhausted"
          else
            match d.desc with
            | Core_ir.LetDecl { name; is_rec; value } ->
                if is_rec then
                  match value.desc with
                  | Fun { param; body } ->
                      let rec_env = Env.extend_rec name param body cur_env in
                      let closure_val = Value.closure ~param ~body ~env:rec_env in
                      loop rec_env cur_fuel (cur_steps + 1) closure_val rest
                  | _ -> (
                      let* v, steps', fuel' =
                        eval_step cur_env cur_fuel cur_steps value
                      in
                      match v with
                      | Value.VClosure { param; body; env = c_env } ->
                          let rec_env = Env.extend_rec name param body c_env in
                          let closure_val = Value.closure ~param ~body ~env:rec_env in
                          let new_env = Env.extend name closure_val cur_env in
                          loop new_env fuel' steps' closure_val rest
                      | non_closure ->
                          let new_env = Env.extend name non_closure cur_env in
                          loop new_env fuel' steps' non_closure rest)
                else
                  let* v, steps', fuel' = eval_step cur_env cur_fuel cur_steps value in
                  let new_env = Env.extend name v cur_env in
                  loop new_env fuel' steps' v rest
            | Core_ir.ExprDecl e ->
                let* v, steps', fuel' = eval_step cur_env cur_fuel cur_steps e in
                loop cur_env fuel' steps' v rest)
    in
    loop env fuel 0 Value.unit p.decls
  with exn -> err p.span (Printf.sprintf "Runtime failure: %s" (Printexc.to_string exn))

let eval_program ?fuel ?env (p : Core_ir.program) : (Value.t, Ast.error) result =
  match eval_program_with_stats ?fuel ?env p with
  | Ok outcome -> Ok outcome.value
  | Error err -> Error err

let eval_expr_with_stats ?(fuel = default_fuel) (env : Env.t) (e : Core_ir.expr) :
    (eval_result, Ast.error) result =
  try
    match eval_step env fuel 0 e with
    | Ok (value, steps, _) -> Ok { value; steps }
    | Error err -> Error err
  with exn -> err e.span (Printf.sprintf "Runtime failure: %s" (Printexc.to_string exn))

let eval_expr ?fuel (env : Env.t) (e : Core_ir.expr) : (Value.t, Ast.error) result =
  match eval_expr_with_stats ?fuel env e with
  | Ok res -> Ok res.value
  | Error err -> Error err
