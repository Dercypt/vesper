type severity = Error | Warning | Note

type t = {
  code : Error_code.t;
  severity : severity;
  span : Ast.span;
  message : string;
  hint : string option;
}

let make ~code ~severity ~span ?hint message = { code; severity; span; message; hint }
let error ~code ~span ?hint message = make ~code ~severity:Error ~span ?hint message
let warning ~code ~span ?hint message = make ~code ~severity:Warning ~span ?hint message
let note ~code ~span ?hint message = make ~code ~severity:Note ~span ?hint message

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

let to_ast_error (diag : t) : Ast.error = { span = diag.span; message = diag.message }

let of_ast_error ?code ?hint (err : Ast.error) : t =
  let code =
    match code with
    | Some c -> c
    | None ->
        let msg = err.message in
        let msg_len = String.length msg in
        if msg_len >= 7 && String.sub msg 0 (min 7 msg_len) = "Invalid" then
          if msg_len >= 12 && String.sub msg 0 (min 12 msg_len) = "Invalid sour" then
            Error_code.E1007_invalid_span
          else Error_code.E1004_invalid_escape
        else if msg_len >= 12 && String.sub msg 0 (min 12 msg_len) = "Unterminated" then
          Error_code.E1002_unterminated_string
        else if msg_len >= 8 && String.sub msg 0 (min 8 msg_len) = "Unclosed" then
          Error_code.E1003_unclosed_comment
        else if msg_len >= 10 && String.sub msg 0 (min 10 msg_len) = "Unexpected" then
          Error_code.E1005_unexpected_char
        else if msg_len >= 7 && String.sub msg 0 (min 7 msg_len) = "Integer" then
          Error_code.E1006_integer_overflow
        else Error_code.E1008_syntax_error
  in
  let hint =
    match hint with
    | Some _ -> hint
    | None -> (
        match code with
        | Error_code.E1002_unterminated_string ->
            Some "Ensure the string literal is closed with a double quote '\"'."
        | Error_code.E1003_unclosed_comment ->
            Some "Ensure the block comment is closed with '*/'."
        | Error_code.E1004_invalid_escape ->
            Some "Supported character escapes are \\n, \\t, \\r, \\\\, and \\\"."
        | Error_code.E1006_integer_overflow ->
            Some "Integer literals must fit within host system integer boundaries."
        | Error_code.E1007_invalid_span ->
            Some "Source span coordinates must be topologically valid."
        | _ -> None)
  in
  { code; severity = Error; span = err.span; message = err.message; hint }

let extract_line ~source ~line =
  if line < 1 then None
  else
    let len = String.length source in
    let rec find_line cur_line idx start_idx =
      if idx >= len then
        if cur_line = line && start_idx < len then
          Some (String.sub source start_idx (len - start_idx))
        else None
      else
        let c = String.get source idx in
        if c = '\n' then
          if cur_line = line then
            let end_idx =
              if idx > start_idx && String.get source (idx - 1) = '\r' then idx - 1
              else idx
            in
            Some (String.sub source start_idx (end_idx - start_idx))
          else find_line (cur_line + 1) (idx + 1) (idx + 1)
        else find_line cur_line (idx + 1) start_idx
    in
    find_line 1 0 0

let render_terminal ?(use_color = false) ?source diag =
  let red s = if use_color then "\027[1;31m" ^ s ^ "\027[0m" else s in
  let yellow s = if use_color then "\027[1;33m" ^ s ^ "\027[0m" else s in
  let cyan s = if use_color then "\027[1;36m" ^ s ^ "\027[0m" else s in
  let blue s = if use_color then "\027[1;34m" ^ s ^ "\027[0m" else s in
  let bold s = if use_color then "\027[1m" ^ s ^ "\027[0m" else s in
  let sev_colored =
    match diag.severity with
    | Error -> red ("error[" ^ Error_code.to_code_string diag.code ^ "]")
    | Warning -> yellow ("warning[" ^ Error_code.to_code_string diag.code ^ "]")
    | Note -> cyan ("note[" ^ Error_code.to_code_string diag.code ^ "]")
  in
  let header = Printf.sprintf "%s: %s" sev_colored (bold diag.message) in
  let location = Printf.sprintf "  %s %s" (blue "-->") (Ast.span_to_string diag.span) in
  let snippet_lines =
    match source with
    | None -> []
    | Some src -> (
        let line_num = diag.span.start_line in
        match extract_line ~source:src ~line:line_num with
        | None -> []
        | Some line_content ->
            let gutter_width = max 2 (String.length (string_of_int line_num)) in
            let empty_gutter = Printf.sprintf "%*s %s" gutter_width "" (blue "|") in
            let code_gutter =
              Printf.sprintf "%*d %s %s" gutter_width line_num (blue "|") line_content
            in
            let start_col = max 0 diag.span.start_col in
            let end_col =
              if diag.span.end_line = diag.span.start_line then
                max (start_col + 1) diag.span.end_col
              else max (start_col + 1) (String.length line_content)
            in
            let count = max 1 (end_col - start_col) in
            let pad = String.make start_col ' ' in
            let carets =
              let c = String.make count '^' in
              match diag.severity with
              | Error -> red c
              | Warning -> yellow c
              | Note -> cyan c
            in
            let pointer_gutter =
              Printf.sprintf "%*s %s %s%s" gutter_width "" (blue "|") pad carets
            in
            [ empty_gutter; code_gutter; pointer_gutter ])
  in
  let hint_lines =
    match diag.hint with
    | None -> []
    | Some h ->
        let gutter_width = max 2 (String.length (string_of_int diag.span.start_line)) in
        let hint_prefix = cyan "= hint:" in
        [ Printf.sprintf "%*s %s %s" gutter_width "" hint_prefix h ]
  in
  String.concat "\n" ((header :: location :: snippet_lines) @ hint_lines)

let pp ?use_color ?source fmt diag =
  Format.pp_print_string fmt (render_terminal ?use_color ?source diag)
