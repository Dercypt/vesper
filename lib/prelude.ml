let prelude_source =
  "let abs x = if x < 0 then -x else x;\n\
   let min x y = if x < y then x else y;\n\
   let max x y = if x > y then x else y;\n\
   let clamp low high x = if x < low then low else if x > high then high else x;\n\
   let sign x = if x < 0 then -1 else if x > 0 then 1 else 0;\n\
   let succ x = x + 1;\n\
   let pred x = x - 1;\n\
   let is_zero x = x == 0;\n\
   let is_positive x = x > 0;\n\
   let is_negative x = x < 0;\n\
   let not b = if b then false else true;\n\
   let id x = x;\n\
   let const x y = x;\n\
   let flip f x y = f y x;\n\
   let compose f g x = f (g x);\n"

let cached_ast =
  lazy
    (match Parse_facade.parse_string ~file:"<prelude>" prelude_source with
    | Ok ast -> ast
    | Error diag -> failwith ("Failed to parse prelude: " ^ diag.message))

let prelude_ast () = Lazy.force cached_ast

let cached_core =
  lazy
    (match Desugar.lower (prelude_ast ()) with
    | Ok core -> core
    | Error err -> failwith ("Failed to lower prelude: " ^ err.message))

let prelude_core () = Lazy.force cached_core

let cached_type_env =
  lazy
    (match Typecheck.typecheck_program (prelude_ast ()) with
    | Ok typed_prog -> typed_prog.env
    | Error diags ->
        let msgs =
          String.concat "; " (List.map (fun (d : Diagnostic.t) -> d.message) diags)
        in
        failwith ("Failed to typecheck prelude: " ^ msgs))

let default_type_env () = Lazy.force cached_type_env

let cached_eval_env =
  lazy
    (match Eval.eval_program_with_stats (prelude_core ()) with
    | Ok outcome -> outcome.env
    | Error err -> failwith ("Failed to evaluate prelude: " ^ err.message))

let default_eval_env () = Lazy.force cached_eval_env

let with_prelude (prog : Ast.program) : Ast.program =
  let p_ast = prelude_ast () in
  { span = prog.span; decls = p_ast.decls @ prog.decls }

let with_prelude_core (prog : Core_ir.program) : Core_ir.program =
  let p_core = prelude_core () in
  { span = prog.span; decls = p_core.decls @ prog.decls }
