let ( let* ) res f = match res with Ok v -> f v | Error e -> Error e

type local = { name : string; depth : int; slot : int }

type builder = {
  code : Opcode.t Dynarray.t;
  spans : Ast.span Dynarray.t;
  constants : Value.t Dynarray.t;
  children : Bytecode.chunk Dynarray.t;
  locals : local list ref;
  scope_depth : int ref;
  stack_depth : int ref;
  param : string option;
  body : Core_ir.expr option;
  captures : (string * int) array;
}

let create_builder ?param ?body ?(captures = [||]) ?(init_depth = 0) () =
  {
    code = Dynarray.create ();
    spans = Dynarray.create ();
    constants = Dynarray.create ();
    children = Dynarray.create ();
    locals = ref [];
    scope_depth = ref 0;
    stack_depth = ref init_depth;
    param;
    body;
    captures;
  }

let add_constant builder (v : Value.t) : int =
  let len = Dynarray.length builder.constants in
  let rec find i =
    if i >= len then None
    else if Value.equal (Dynarray.get builder.constants i) v then Some i
    else find (i + 1)
  in
  match find 0 with
  | Some idx -> idx
  | None ->
      let idx = len in
      Dynarray.add_last builder.constants v;
      idx

let emit builder ~span (op : Opcode.t) : int =
  let idx = Dynarray.length builder.code in
  Dynarray.add_last builder.code op;
  Dynarray.add_last builder.spans span;
  idx

let add_local builder name ~slot =
  let l = { name; depth = !(builder.scope_depth); slot } in
  builder.locals := l :: !(builder.locals);
  slot

let resolve_local builder name =
  let rec find = function
    | [] -> None
    | l :: rest -> if String.equal l.name name then Some l.slot else find rest
  in
  find !(builder.locals)

let enter_scope builder = incr builder.scope_depth

let exit_scope builder =
  decr builder.scope_depth;
  builder.locals :=
    List.filter (fun l -> l.depth <= !(builder.scope_depth)) !(builder.locals)

let build_chunk builder : Bytecode.chunk =
  Bytecode.create
    ~code:(Dynarray.to_array builder.code)
    ~constants:(Dynarray.to_array builder.constants)
    ~spans:(Dynarray.to_array builder.spans)
    ~children:(Dynarray.to_array builder.children)
    ?param:builder.param ?body:builder.body ~captures:builder.captures ()

let rec compile_fun ?(captures = [||]) ~param (body : Core_ir.expr) : Bytecode.chunk =
  let builder = create_builder ~param ~body ~captures ~init_depth:1 () in
  ignore (add_local builder param ~slot:0);
  (match compile_expr_into builder body with Ok () -> () | Error _ -> ());
  ignore (emit builder ~span:body.span Opcode.Op_Return);
  build_chunk builder

and compile_expr_into builder (e : Core_ir.expr) : (unit, Ast.error) result =
  match e.desc with
  | Lit (Int n) ->
      let c = add_constant builder (Value.VInt n) in
      ignore (emit builder ~span:e.span (Opcode.Op_Const c));
      incr builder.stack_depth;
      Ok ()
  | Lit (Bool b) ->
      let c = add_constant builder (Value.VBool b) in
      ignore (emit builder ~span:e.span (Opcode.Op_Const c));
      incr builder.stack_depth;
      Ok ()
  | Lit (String s) ->
      let c = add_constant builder (Value.VString s) in
      ignore (emit builder ~span:e.span (Opcode.Op_Const c));
      incr builder.stack_depth;
      Ok ()
  | Var name ->
      (match resolve_local builder name with
      | Some slot -> ignore (emit builder ~span:e.span (Opcode.Op_GetLocal slot))
      | None -> ignore (emit builder ~span:e.span (Opcode.Op_GetGlobal name)));
      incr builder.stack_depth;
      Ok ()
  | If { cond; then_branch; else_branch } ->
      let* () = compile_expr_into builder cond in
      let jump_false_idx = emit builder ~span:cond.span (Opcode.Op_JumpIfFalse (-1)) in
      decr builder.stack_depth;
      let depth_before = !(builder.stack_depth) in
      let* () = compile_expr_into builder then_branch in
      let jump_end_idx = emit builder ~span:then_branch.span (Opcode.Op_Jump (-1)) in
      let else_target = Dynarray.length builder.code in
      Dynarray.set builder.code jump_false_idx (Opcode.Op_JumpIfFalse else_target);
      builder.stack_depth := depth_before;
      let* () = compile_expr_into builder else_branch in
      let end_target = Dynarray.length builder.code in
      Dynarray.set builder.code jump_end_idx (Opcode.Op_Jump end_target);
      Ok ()
  | PrimOp { op; args } -> (
      let rec compile_args = function
        | [] -> Ok ()
        | a :: rest ->
            let* () = compile_expr_into builder a in
            compile_args rest
      in
      let* () = compile_args args in
      let emit_op op_code =
        ignore (emit builder ~span:e.span op_code);
        builder.stack_depth := !(builder.stack_depth) - List.length args + 1;
        Ok ()
      in
      match (op, List.length args) with
      | Neg, 1 -> emit_op Opcode.Op_Neg
      | Not, 1 -> emit_op Opcode.Op_Not
      | Add, 2 -> emit_op Opcode.Op_Add
      | Sub, 2 -> emit_op Opcode.Op_Sub
      | Mul, 2 -> emit_op Opcode.Op_Mul
      | Div, 2 -> emit_op Opcode.Op_Div
      | Mod, 2 -> emit_op Opcode.Op_Mod
      | Eq, 2 -> emit_op Opcode.Op_Eq
      | Neq, 2 -> emit_op Opcode.Op_Neq
      | Lt, 2 -> emit_op Opcode.Op_Lt
      | Le, 2 -> emit_op Opcode.Op_Le
      | Gt, 2 -> emit_op Opcode.Op_Gt
      | Ge, 2 -> emit_op Opcode.Op_Ge
      | _ ->
          Error
            {
              Ast.span = e.span;
              message =
                Printf.sprintf "Invalid primop arity for %s" (Core_ir.primop_to_string op);
            })
  | App { fn; arg } ->
      let* () = compile_expr_into builder fn in
      let* () = compile_expr_into builder arg in
      ignore (emit builder ~span:fn.span (Opcode.Op_Call 1));
      builder.stack_depth := !(builder.stack_depth) - 1;
      Ok ()
  | Fun { param; body } ->
      let captures =
        List.rev_map (fun l -> (l.name, l.slot)) !(builder.locals) |> Array.of_list
      in
      let child_chunk = compile_fun ~captures ~param body in
      let child_idx = Dynarray.length builder.children in
      Dynarray.add_last builder.children child_chunk;
      ignore (emit builder ~span:e.span (Opcode.Op_Closure child_idx));
      incr builder.stack_depth;
      Ok ()
  | Let { name; is_rec; value; body } ->
      let slot = !(builder.stack_depth) in
      let* () = compile_expr_into builder value in
      if is_rec then ignore (emit builder ~span:value.span (Opcode.Op_TieRec name));
      enter_scope builder;
      ignore (add_local builder name ~slot);
      let* () = compile_expr_into builder body in
      exit_scope builder;
      ignore (emit builder ~span:e.span (Opcode.Op_SetLocal slot));
      builder.stack_depth := slot + 1;
      Ok ()

let compile_expr ?(env = Env.empty) (e : Core_ir.expr) :
    (Bytecode.chunk, Ast.error) result =
  let* () = Core_ir.validate_expr e in
  ignore env;
  let builder = create_builder ~body:e () in
  let* () = compile_expr_into builder e in
  ignore (emit builder ~span:e.span Opcode.Op_Halt);
  Ok (build_chunk builder)

let compile_decl ?(env = Env.empty) (d : Core_ir.decl) :
    (Bytecode.chunk, Ast.error) result =
  let* () = Core_ir.validate_decl d in
  ignore env;
  let builder = create_builder () in
  let* () =
    match d.Core_ir.desc with
    | LetDecl { name; is_rec; value } ->
        let* () = compile_expr_into builder value in
        if is_rec then ignore (emit builder ~span:value.span (Opcode.Op_TieRec name));
        ignore (emit builder ~span:d.span (Opcode.Op_DefGlobal name));
        Ok ()
    | ExprDecl e -> compile_expr_into builder e
  in
  ignore (emit builder ~span:d.span Opcode.Op_Halt);
  Ok (build_chunk builder)

let compile_program ?(env = Env.empty) (p : Core_ir.program) :
    (Bytecode.chunk, Ast.error) result =
  let* () = Core_ir.validate_program p in
  ignore env;
  let builder = create_builder () in
  let rec compile_decls = function
    | [] ->
        ignore (emit builder ~span:p.span Opcode.Op_Unit);
        ignore (emit builder ~span:p.span Opcode.Op_Halt);
        Ok ()
    | [ (d : Core_ir.decl) ] ->
        let* () =
          match d.Core_ir.desc with
          | LetDecl { name; is_rec; value } ->
              let* () = compile_expr_into builder value in
              if is_rec then
                ignore (emit builder ~span:value.span (Opcode.Op_TieRec name));
              ignore (emit builder ~span:d.span (Opcode.Op_DefGlobal name));
              Ok ()
          | ExprDecl e -> compile_expr_into builder e
        in
        ignore (emit builder ~span:d.span Opcode.Op_Halt);
        Ok ()
    | (d : Core_ir.decl) :: rest ->
        let* () =
          match d.Core_ir.desc with
          | LetDecl { name; is_rec; value } ->
              let* () = compile_expr_into builder value in
              if is_rec then
                ignore (emit builder ~span:value.span (Opcode.Op_TieRec name));
              ignore (emit builder ~span:d.span (Opcode.Op_DefGlobal name));
              ignore (emit builder ~span:d.span Opcode.Op_Pop);
              builder.stack_depth := 0;
              Ok ()
          | ExprDecl e ->
              let* () = compile_expr_into builder e in
              ignore (emit builder ~span:d.span Opcode.Op_Pop);
              builder.stack_depth := 0;
              Ok ()
        in
        compile_decls rest
  in
  let* () = compile_decls p.decls in
  Ok (build_chunk builder)

let compile (ast : Ast.program) : (Bytecode.chunk, Ast.error) result =
  let* core = Desugar.lower ast in
  compile_program core
