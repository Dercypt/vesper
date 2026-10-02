let tokenize ~file input : (Tokens.token list, Ast.error) result =
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = 1; pos_bol = 0 };
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> Ok (List.rev (Parser.EOF :: acc))
    | tok -> loop (tok :: acc)
    | exception Lexer.Error err -> Error err
    | exception exn ->
        let span =
          Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
        in
        Error { Ast.span; message = Printexc.to_string exn }
  in
  loop []

let parse_string ~file input : (Ast.program, Ast.error) result =
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = 1; pos_bol = 0 };
  try
    let prog = Parser.program Lexer.token lexbuf in
    match Ast.validate_program prog with Ok () -> Ok prog | Error e -> Error e
  with
  | Lexer.Error err -> Error err
  | Parser.Error ->
      let span =
        Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      let lexeme = Lexing.lexeme lexbuf in
      let message =
        if String.length lexeme = 0 then "Unexpected end of input"
        else Printf.sprintf "Syntax error near %S" lexeme
      in
      Error { Ast.span; message }
  | exn ->
      let span =
        Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Error { Ast.span; message = Printexc.to_string exn }

let parse_expr ~file input : (Ast.expr, Ast.error) result =
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = 1; pos_bol = 0 };
  try
    let expr = Parser.expr_entry Lexer.token lexbuf in
    match Ast.validate_expr expr with Ok () -> Ok expr | Error e -> Error e
  with
  | Lexer.Error err -> Error err
  | Parser.Error ->
      let span =
        Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      let lexeme = Lexing.lexeme lexbuf in
      let message =
        if String.length lexeme = 0 then "Unexpected end of input"
        else Printf.sprintf "Syntax error near %S" lexeme
      in
      Error { Ast.span; message }
  | exn ->
      let span =
        Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Error { Ast.span; message = Printexc.to_string exn }
