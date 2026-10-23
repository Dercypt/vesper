let read_file path : (string, Diagnostic.t) result =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Ok (In_channel.input_all ic))
  with
  | Sys_error msg ->
      let span = Ast.dummy_span in
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "Cannot read file %S: %s" path msg))
  | exn ->
      let span = Ast.dummy_span in
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "Failed reading file %S: %s" path (Printexc.to_string exn)))

let compile_string ?(file = "<input>") input : (Ast.program, Diagnostic.t) result =
  try Parse_facade.parse_string ~file input
  with exn ->
    let span = Ast.dummy_span in
    Error
      (Diagnostic.error ~code:Error_code.E0002_internal_error ~span
         (Printf.sprintf "Fatal compiler exception: %s" (Printexc.to_string exn)))

let compile_file path : (Ast.program, Diagnostic.t) result =
  match read_file path with
  | Error diag -> Error diag
  | Ok content -> compile_string ~file:path content
