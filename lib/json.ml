type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

exception Fail of string

let max_depth = 256

let parse_exn (s : string) : t =
  let n = String.length s in
  let pos = ref 0 in
  let fail msg = raise (Fail (Printf.sprintf "%s at offset %d" msg !pos)) in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let advance () = incr pos in
  let rec skip_ws () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        advance ();
        skip_ws ()
    | _ -> ()
  in
  let expect_lit lit value =
    let l = String.length lit in
    if !pos + l <= n && String.sub s !pos l = lit then begin
      pos := !pos + l;
      value
    end
    else fail "Invalid literal"
  in
  let hex4 () =
    if !pos + 4 > n then fail "Truncated unicode escape";
    let v =
      match int_of_string_opt ("0x" ^ String.sub s !pos 4) with
      | Some v -> v
      | None -> fail "Invalid unicode escape"
    in
    pos := !pos + 4;
    v
  in
  let parse_string () =
    advance ();
    let buf = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | None -> fail "Unterminated string"
      | Some '"' -> advance ()
      | Some '\\' ->
          advance ();
          (match peek () with
          | None -> fail "Unterminated escape"
          | Some c -> (
              advance ();
              match c with
              | '"' -> Buffer.add_char buf '"'
              | '\\' -> Buffer.add_char buf '\\'
              | '/' -> Buffer.add_char buf '/'
              | 'b' -> Buffer.add_char buf '\b'
              | 'f' -> Buffer.add_char buf '\012'
              | 'n' -> Buffer.add_char buf '\n'
              | 'r' -> Buffer.add_char buf '\r'
              | 't' -> Buffer.add_char buf '\t'
              | 'u' ->
                  let hi = hex4 () in
                  let cp =
                    if hi >= 0xD800 && hi <= 0xDBFF then
                      if !pos + 6 <= n && s.[!pos] = '\\' && s.[!pos + 1] = 'u' then begin
                        pos := !pos + 2;
                        let lo = hex4 () in
                        if lo >= 0xDC00 && lo <= 0xDFFF then
                          0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00)
                        else 0xFFFD
                      end
                      else 0xFFFD
                    else if hi >= 0xDC00 && hi <= 0xDFFF then 0xFFFD
                    else hi
                  in
                  Buffer.add_utf_8_uchar buf (Uchar.of_int cp)
              | _ -> fail "Invalid escape"));
          loop ()
      | Some c when Char.code c < 0x20 -> fail "Control character in string"
      | Some c ->
          Buffer.add_char buf c;
          advance ();
          loop ()
    in
    loop ();
    Buffer.contents buf
  in
  let parse_number () =
    let start = !pos in
    let is_float = ref false in
    let rec loop () =
      match peek () with
      | Some ('0' .. '9' | '-' | '+') ->
          advance ();
          loop ()
      | Some ('.' | 'e' | 'E') ->
          is_float := true;
          advance ();
          loop ()
      | _ -> ()
    in
    loop ();
    let text = String.sub s start (!pos - start) in
    if !is_float then
      match float_of_string_opt text with
      | Some f -> Float f
      | None -> fail "Invalid number"
    else
      match int_of_string_opt text with Some i -> Int i | None -> fail "Invalid number"
  in
  let rec parse_value depth =
    if depth > max_depth then fail "Nesting too deep";
    skip_ws ();
    match peek () with
    | None -> fail "Unexpected end of input"
    | Some 'n' -> expect_lit "null" Null
    | Some 't' -> expect_lit "true" (Bool true)
    | Some 'f' -> expect_lit "false" (Bool false)
    | Some '"' -> String (parse_string ())
    | Some '[' ->
        advance ();
        skip_ws ();
        if peek () = Some ']' then begin
          advance ();
          List []
        end
        else
          let rec items acc =
            let v = parse_value (depth + 1) in
            skip_ws ();
            match peek () with
            | Some ',' ->
                advance ();
                items (v :: acc)
            | Some ']' ->
                advance ();
                List (List.rev (v :: acc))
            | _ -> fail "Expected ',' or ']'"
          in
          items []
    | Some '{' ->
        advance ();
        skip_ws ();
        if peek () = Some '}' then begin
          advance ();
          Object []
        end
        else
          let rec fields acc =
            skip_ws ();
            if peek () <> Some '"' then fail "Expected object key";
            let k = parse_string () in
            skip_ws ();
            if peek () <> Some ':' then fail "Expected ':'";
            advance ();
            let v = parse_value (depth + 1) in
            skip_ws ();
            match peek () with
            | Some ',' ->
                advance ();
                fields ((k, v) :: acc)
            | Some '}' ->
                advance ();
                Object (List.rev ((k, v) :: acc))
            | _ -> fail "Expected ',' or '}'"
          in
          fields []
    | Some ('0' .. '9' | '-') -> parse_number ()
    | Some _ -> fail "Unexpected character"
  in
  let v = parse_value 0 in
  skip_ws ();
  if !pos <> n then fail "Trailing characters after JSON value";
  v

let parse s = try Ok (parse_exn s) with Fail msg -> Error msg

let escape_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let to_string v =
  let buf = Buffer.create 64 in
  let rec go = function
    | Null -> Buffer.add_string buf "null"
    | Bool b -> Buffer.add_string buf (if b then "true" else "false")
    | Int i -> Buffer.add_string buf (string_of_int i)
    | Float f -> Buffer.add_string buf (Printf.sprintf "%.17g" f)
    | String s -> escape_string buf s
    | List l ->
        Buffer.add_char buf '[';
        List.iteri
          (fun i x ->
            if i > 0 then Buffer.add_char buf ',';
            go x)
          l;
        Buffer.add_char buf ']'
    | Object fs ->
        Buffer.add_char buf '{';
        List.iteri
          (fun i (k, x) ->
            if i > 0 then Buffer.add_char buf ',';
            escape_string buf k;
            Buffer.add_char buf ':';
            go x)
          fs;
        Buffer.add_char buf '}'
  in
  go v;
  Buffer.contents buf

let member key = function Object fs -> List.assoc_opt key fs | _ -> None
let to_string_opt = function String s -> Some s | _ -> None
let to_int_opt = function Int i -> Some i | _ -> None
let to_list_opt = function List l -> Some l | _ -> None
