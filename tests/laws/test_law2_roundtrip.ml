open Vesper
open Vesper.Ast

let dummy_loc : span =
  match
    create_span ~file:"generated.vesper" ~start_line:1 ~start_col:0 ~end_line:1 ~end_col:1
  with
  | Ok s -> s
  | Error _ -> Ast.dummy_span

let var_pool = [| "x"; "y"; "z"; "a"; "b"; "c"; "f"; "g"; "n"; "acc"; "val_1" |]
let random_var () = var_pool.(Random.int (Array.length var_pool))

let random_string () =
  let len = 1 + Random.int 8 in
  let chars = [| 'a'; 'b'; 'c'; '1'; '2'; ' '; '_'; '-'; '\t'; '\n'; '\\'; '"' |] in
  let b = Buffer.create len in
  for _ = 1 to len do
    let c = chars.(Random.int (Array.length chars)) in
    Buffer.add_char b c
  done;
  Buffer.contents b

let random_binop () =
  match Random.int 13 with
  | 0 -> Add
  | 1 -> Sub
  | 2 -> Mul
  | 3 -> Div
  | 4 -> Mod
  | 5 -> Eq
  | 6 -> Neq
  | 7 -> Lt
  | 8 -> Le
  | 9 -> Gt
  | 10 -> Ge
  | 11 -> And
  | _ -> Or

let random_unop () = if Random.bool () then Neg else Not

let rec gen_expr depth : expr =
  if depth <= 0 then gen_leaf ()
  else
    match Random.int 8 with
    | 0 -> gen_leaf ()
    | 1 ->
        let op = random_unop () in
        let arg = gen_expr (depth - 1) in
        { span = dummy_loc; desc = Unary { op; arg } }
    | 2 ->
        let op = random_binop () in
        let lhs = gen_expr (depth - 1) in
        let rhs = gen_expr (depth - 1) in
        { span = dummy_loc; desc = Binary { op; lhs; rhs } }
    | 3 ->
        let cond = gen_expr (depth - 1) in
        let then_branch = gen_expr (depth - 1) in
        let else_branch = gen_expr (depth - 1) in
        { span = dummy_loc; desc = If { cond; then_branch; else_branch } }
    | 4 ->
        let name = random_var () in
        let is_rec = Random.bool () in
        let num_args = Random.int 3 in
        let args = List.init num_args (fun _ -> random_var ()) in
        let value = gen_expr (depth - 1) in
        let body = gen_expr (depth - 1) in
        { span = dummy_loc; desc = Let { name; is_rec; args; value; body } }
    | 5 ->
        let num_params = 1 + Random.int 3 in
        let params = List.init num_params (fun _ -> random_var ()) in
        let body = gen_expr (depth - 1) in
        { span = dummy_loc; desc = Fun { params; body } }
    | 6 ->
        let fn = gen_expr (depth - 1) in
        let arg = gen_expr (depth - 1) in
        { span = dummy_loc; desc = App { fn; arg } }
    | _ -> gen_leaf ()

and gen_leaf () : expr =
  let desc =
    match Random.int 4 with
    | 0 -> Lit (Int (Random.int 10000))
    | 1 -> Lit (Bool (Random.bool ()))
    | 2 -> Lit (String (random_string ()))
    | _ -> Var (random_var ())
  in
  { span = dummy_loc; desc }

let gen_decl depth : decl =
  let desc =
    if Random.bool () then
      let name = random_var () in
      let is_rec = Random.bool () in
      let num_args = Random.int 3 in
      let args = List.init num_args (fun _ -> random_var ()) in
      let value = gen_expr depth in
      LetDecl { name; is_rec; args; value }
    else ExprDecl (gen_expr depth)
  in
  { span = dummy_loc; decl_desc = desc }

let gen_program depth : program =
  let count = 1 + Random.int 4 in
  let decls = List.init count (fun _ -> gen_decl depth) in
  { span = dummy_loc; decls }

let test_roundtrip_precedence_handcrafted () =
  let cases =
    [
      "1 + 2 * 3;";
      "(1 + 2) * 3;";
      "1 - 2 - 3;";
      "1 - (2 - 3);";
      "1 / 2 / 3;";
      "1 / (2 / 3);";
      "a && b || c;";
      "a && (b || c);";
      "a == b && c != d;";
      "!a && b;";
      "!(a && b);";
      "f x y;";
      "f (g x);";
      "(f x) y;";
      "f (1 + 2);";
      "(fun x -> x) 42;";
      "let x = 1 in x + 1;";
      "let rec f x = if x == 0 then 1 else x * f (x - 1) in f 5;";
      "fun x y -> x + y;";
      "if true then 1 else if false then 2 else 3;";
      "if (if a then b else c) then d else e;";
      "let x = \"hello \\\"world\\\" \\n tab\\t\";";
      "let f x y = x + y;\nlet g z = f z z;\ng 10;";
    ]
  in
  List.iter
    (fun src ->
      match Parse_facade.parse_string ~file:"handcrafted.vesper" src with
      | Error err ->
          Alcotest.fail
            (Printf.sprintf "Failed to parse initial input %S: %s" src err.message)
      | Ok prog -> (
          let printed = Printer.to_string prog in
          match
            Parse_facade.parse_string ~file:"handcrafted_roundtrip.vesper" printed
          with
          | Error err ->
              Alcotest.fail
                (Printf.sprintf
                   "Failed to reparse printed output:\n\
                    Original: %s\n\
                    Printed:\n\
                    %s\n\
                    Error: %s"
                   src printed err.message)
          | Ok reparsed ->
              if not (ast_equal prog reparsed) then
                Alcotest.fail
                  (Printf.sprintf "AST mismatch for:\nOriginal: %s\nPrinted:\n%s" src
                     printed)))
    cases

let test_roundtrip_fuzz_1000 () =
  Random.init 42;
  for i = 1 to 1000 do
    let prog = gen_program 3 in
    let printed = Printer.to_string prog in
    match Parse_facade.parse_string ~file:"fuzz_roundtrip.vesper" printed with
    | Error err ->
        Alcotest.fail
          (Printf.sprintf
             "Roundtrip iteration %d failed to parse printed output:\n%s\nError: %s" i
             printed err.message)
    | Ok reparsed ->
        if not (ast_equal prog reparsed) then
          Alcotest.fail
            (Printf.sprintf
               "Roundtrip iteration %d failed structural equality:\n\
                Printed:\n\
                %s\n\
                Reprinted:\n\
                %s"
               i printed (Printer.to_string reparsed))
  done

let test_comments_and_whitespace () =
  let src =
    "// Leading line comment\n\
     let x = 10; // Inline comment\n\
     /* Block comment */\n\
     let y = /* Nested /* comment */ here */ 20;\n\
     x + y;\n"
  in
  match Parse_facade.parse_string ~file:"comments.vesper" src with
  | Error err ->
      Alcotest.fail (Printf.sprintf "Failed to parse input with comments: %s" err.message)
  | Ok prog -> Alcotest.(check int) "Parses 3 declarations" 3 (List.length prog.decls)

let tests =
  [
    Alcotest.test_case "Handcrafted operator precedence & structure roundtrip" `Quick
      test_roundtrip_precedence_handcrafted;
    Alcotest.test_case "Property fuzzing 1,000 random AST roundtrips" `Quick
      test_roundtrip_fuzz_1000;
    Alcotest.test_case "Comment and whitespace tolerance" `Quick
      test_comments_and_whitespace;
  ]
