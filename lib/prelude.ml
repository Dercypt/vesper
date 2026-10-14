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
   let compose f g x = f (g x);\n\
   let add a b = a + b;\n\
   let sub a b = a - b;\n\
   let mul a b = a * b;\n\
   let div a b = if b == 0 then 0 else a / b;\n\
   let rem a b = if b == 0 then 0 else a % b;\n\
   let modulo a b = if b == 0 then 0 else a % b;\n\
   let is_even x = (x % 2) == 0;\n\
   let is_odd x = (x % 2) != 0;\n\
   let rec pow b e = if e <= 0 then 1 else b * pow b (e - 1);\n\
   let rec fact n = if n <= 1 then 1 else n * fact (n - 1);\n\
   let rec gcd a b = if b == 0 then a else gcd b (a % b);\n\
   let lcm a b = if a == 0 || b == 0 then 0 else (a * b) / (gcd a b);\n\
   let rec isqrt_guess n g = if g * g <= n && (g + 1) * (g + 1) > n then g else if g == \
   0 then 0 else isqrt_guess n ((g + n / g) / 2);\n\
   let isqrt n = if n <= 0 then 0 else isqrt_guess n (n / 2 + 1);\n\
   let rec fib n = if n <= 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2);\n\
   let digit_to_str d = if d == 0 then \"0\" else if d == 1 then \"1\" else if d == 2 \
   then \"2\" else if d == 3 then \"3\" else if d == 4 then \"4\" else if d == 5 then \
   \"5\" else if d == 6 then \"6\" else if d == 7 then \"7\" else if d == 8 then \"8\" \
   else if d == 9 then \"9\" else \"?\";\n\
   let rec int_to_str_pos n = if n < 10 then digit_to_str n else \"\" + (int_to_str_pos \
   (n / 10)) + (digit_to_str (n % 10));\n\
   let int_to_str n = if n < 0 then \"-\" + (int_to_str_pos (-n)) else int_to_str_pos n;\n\
   let bool_to_str b = if b then \"true\" else \"false\";\n\
   let string_concat a b = \"\" + a + b;\n\
   let string_is_empty s = s == \"\";\n"

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
