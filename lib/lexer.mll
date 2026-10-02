{
open Parser

exception Error of Ast.error

let lex_error lexbuf msg =
  let span =
    Ast.span_of_positions (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
  in
  raise (Error { Ast.span; message = msg })
}

let whitespace = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let ident_start = alpha | '_'
let ident_char = ident_start | digit

rule token = parse
  | whitespace+
      { token lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; token lexbuf }
  | "//" [^ '\n']*
      { token lexbuf }
  | "/*"
      { block_comment 1 lexbuf; token lexbuf }
  | digit+ as raw
      {
        match int_of_string_opt raw with
        | Some n -> INT n
        | None -> lex_error lexbuf (Printf.sprintf "Integer literal overflow: %s" raw)
      }
  | "true"
      { BOOL true }
  | "false"
      { BOOL false }
  | "let"
      { LET }
  | "rec"
      { REC }
  | "in"
      { IN }
  | "if"
      { IF }
  | "then"
      { THEN }
  | "else"
      { ELSE }
  | "fun"
      { FUN }
  | "match"
      { MATCH }
  | "with"
      { WITH }
  | "=="
      { EQEQ }
  | "!="
      { NOTEQ }
  | "<="
      { LTE }
  | ">="
      { GTE }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "->"
      { ARROW }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { STAR }
  | '/'
      { SLASH }
  | '%'
      { PERCENT }
  | '<'
      { LT }
  | '>'
      { GT }
  | '!'
      { NOT }
  | '='
      { EQUAL }
  | ':'
      { COLON }
  | ';'
      { SEMICOLON }
  | ','
      { COMMA }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | '|'
      { BAR }
  | '"'
      {
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let buf = Buffer.create 32 in
        string_literal start_pos buf lexbuf
      }
  | ident_start ident_char* as raw
      { IDENT raw }
  | eof
      { EOF }
  | _ as c
      { lex_error lexbuf (Printf.sprintf "Unexpected character: %C" c) }

and string_literal start_pos buf = parse
  | '"'
      { STRING (Buffer.contents buf) }
  | "\\n"
      { Buffer.add_char buf '\n'; string_literal start_pos buf lexbuf }
  | "\\t"
      { Buffer.add_char buf '\t'; string_literal start_pos buf lexbuf }
  | "\\r"
      { Buffer.add_char buf '\r'; string_literal start_pos buf lexbuf }
  | "\\\\"
      { Buffer.add_char buf '\\'; string_literal start_pos buf lexbuf }
  | "\\\""
      { Buffer.add_char buf '"'; string_literal start_pos buf lexbuf }
  | '\\' (_ as c)
      {
        lex_error lexbuf (Printf.sprintf "Invalid character escape: \\%c" c)
      }
  | '\n'
      {
        Lexing.new_line lexbuf;
        Buffer.add_char buf '\n';
        string_literal start_pos buf lexbuf
      }
  | [^ '"' '\\' '\n']+ as str
      {
        Buffer.add_string buf str;
        string_literal start_pos buf lexbuf
      }
  | eof
      {
        let span = Ast.span_of_positions (start_pos, Lexing.lexeme_end_p lexbuf) in
        raise (Error { Ast.span; message = "Unterminated string literal" })
      }

and block_comment depth = parse
  | "/*"
      { block_comment (depth + 1) lexbuf }
  | "*/"
      { if depth = 1 then () else block_comment (depth - 1) lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | eof
      { lex_error lexbuf "Unclosed block comment" }
  | _
      { block_comment depth lexbuf }
