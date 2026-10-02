type span = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

type error = { span : span; message : string }
type literal = Int of int | Bool of bool | String of string

type expr = { span : span; desc : expr_desc }
and expr_desc = Lit of literal | Var of string

let dummy_span : span =
  { file = "<dummy>"; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }

let span_to_string (s : span) : string =
  Printf.sprintf "%s:%d:%d-%d:%d" s.file s.start_line s.start_col s.end_line s.end_col

let span_is_valid (s : span) : bool =
  s.start_line >= 1 && s.start_col >= 0 && s.end_line >= s.start_line
  && if s.end_line = s.start_line then s.end_col >= s.start_col else s.end_col >= 0

let create_span ~file ~start_line ~start_col ~end_line ~end_col : (span, error) result =
  let candidate = { file; start_line; start_col; end_line; end_col } in
  if span_is_valid candidate then Ok candidate
  else
    Error
      {
        span = dummy_span;
        message =
          Printf.sprintf "Invalid source coordinates for span: %s"
            (span_to_string candidate);
      }

let validate_expr (e : expr) : (unit, error) result =
  if not (span_is_valid e.span) then
    Error
      {
        span = e.span;
        message =
          Printf.sprintf "AST node violates Law 3 (invalid span provenance): %s"
            (span_to_string e.span);
      }
  else
    match e.desc with
    | Lit (Int _) | Lit (Bool _) | Lit (String _) -> Ok ()
    | Var name ->
        if String.length name = 0 then
          Error { span = e.span; message = "Variable identifier cannot be empty" }
        else Ok ()
