let length (s : string) = String.length s
let is_empty (s : string) = String.length s = 0
let concat ?(sep = "") (parts : string list) = String.concat sep parts

let split ~on (s : string) =
  let len = String.length s in
  let rec find_next start idx acc =
    if idx >= len then
      let sub = String.sub s start (idx - start) in
      List.rev (sub :: acc)
    else if String.get s idx = on then
      let sub = String.sub s start (idx - start) in
      find_next (idx + 1) (idx + 1) (sub :: acc)
    else find_next start (idx + 1) acc
  in
  find_next 0 0 []

let split_lines (s : string) =
  let raw = split ~on:'\n' s in
  List.map
    (fun line ->
      let len = String.length line in
      if len > 0 && String.get line (len - 1) = '\r' then String.sub line 0 (len - 1)
      else line)
    raw

let trim (s : string) = String.trim s

let slice (s : string) ~start ~len =
  let total_len = String.length s in
  if start < 0 || len < 0 || start + len > total_len then Error "Index out of bounds"
  else try Ok (String.sub s start len) with _ -> Error "Index out of bounds"

let sub = slice

let starts_with ~prefix (s : string) =
  let plen = String.length prefix in
  let slen = String.length s in
  if plen > slen then false else String.sub s 0 plen = prefix

let ends_with ~suffix (s : string) =
  let slen = String.length suffix in
  let tlen = String.length s in
  if slen > tlen then false else String.sub s (tlen - slen) slen = suffix

let contains (s : string) ~sub =
  let sub_len = String.length sub in
  let str_len = String.length s in
  if sub_len = 0 then true
  else if sub_len > str_len then false
  else
    let rec loop idx =
      if idx + sub_len > str_len then false
      else if String.sub s idx sub_len = sub then true
      else loop (idx + 1)
    in
    loop 0

let index_of (s : string) ~sub =
  let sub_len = String.length sub in
  let str_len = String.length s in
  if sub_len = 0 then Some 0
  else if sub_len > str_len then None
  else
    let rec loop idx =
      if idx + sub_len > str_len then None
      else if String.sub s idx sub_len = sub then Some idx
      else loop (idx + 1)
    in
    loop 0

let of_int (n : int) = string_of_int n
let to_int (s : string) = int_of_string_opt s
let of_bool (b : bool) = string_of_bool b
let to_bool (s : string) = bool_of_string_opt s

let to_char_list (s : string) =
  let len = String.length s in
  let rec loop idx acc =
    if idx < 0 then acc else loop (idx - 1) (String.get s idx :: acc)
  in
  loop (len - 1) []

let of_char_list (chars : char list) =
  let buf = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
