module I = Parser.MenhirInterpreter

let supplier_of_lexbuf lexbuf =
  let next_token () =
    let tok = Lexer.token lexbuf in
    let start_pos = Lexing.lexeme_start_p lexbuf in
    let end_pos = Lexing.lexeme_end_p lexbuf in
    (tok, start_pos, end_pos)
  in
  next_token

let run_parser checkpoint ~file input =
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = 1; pos_bol = 0 };
  let supplier = supplier_of_lexbuf lexbuf in
  let succeed v = Ok v in
  let fail checkpoint =
    let span, message, hint =
      match checkpoint with
      | I.HandlingError env ->
          let state = I.current_state_number env in
          let start_p, end_p = I.positions env in
          let span = Ast.span_of_positions (start_p, end_p) in
          let raw_msg =
            try String.trim (Parser_messages.message state)
            with Not_found -> "Syntax error"
          in
          let lexeme = Lexing.lexeme lexbuf in
          let msg =
            if String.length lexeme > 0 then Printf.sprintf "%s (near %S)" raw_msg lexeme
            else raw_msg
          in
          let hint =
            if String.length lexeme = 0 then
              Some "Syntax is incomplete; unexpected end of input."
            else None
          in
          (span, msg, hint)
      | _ ->
          let span =
            Ast.span_of_positions
              (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
          in
          (span, "Syntax error", None)
    in
    Error (Diagnostic.error ~code:Error_code.E1001_unexpected_token ~span ?hint message)
  in
  try I.loop_handle succeed fail supplier checkpoint with
  | Lexer.Error err -> Error (Diagnostic.of_ast_error err)
  | exn ->
      let span =
        Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      Error
        (Diagnostic.error ~code:Error_code.E0002_internal_error ~span
           (Printexc.to_string exn))

let parse_string ~file input : (Ast.program, Diagnostic.t) result =
  let init_pos = { Lexing.pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  match run_parser (Parser.Incremental.program init_pos) ~file input with
  | Ok prog -> (
      match Ast.validate_program prog with
      | Ok () -> Ok prog
      | Error err ->
          Error (Diagnostic.of_ast_error ~code:Error_code.E1007_invalid_span err))
  | Error diag -> Error diag

let parse_expr ~file input : (Ast.expr, Diagnostic.t) result =
  let init_pos = { Lexing.pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  match run_parser (Parser.Incremental.expr_entry init_pos) ~file input with
  | Ok expr -> (
      match Ast.validate_expr expr with
      | Ok () -> Ok expr
      | Error err ->
          Error (Diagnostic.of_ast_error ~code:Error_code.E1007_invalid_span err))
  | Error diag -> Error diag

let tokenize ~file input : (Tokens.token list, Diagnostic.t) result =
  let lexbuf = Lexing.from_string input in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = 1; pos_bol = 0 };
  let rec loop acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> Ok (List.rev (Parser.EOF :: acc))
    | tok -> loop (tok :: acc)
    | exception Lexer.Error err -> Error (Diagnostic.of_ast_error err)
    | exception exn ->
        let span =
          Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
        in
        Error
          (Diagnostic.error ~code:Error_code.E0002_internal_error ~span
             (Printexc.to_string exn))
  in
  loop []
