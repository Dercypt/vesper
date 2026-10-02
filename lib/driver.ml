let read_file path : (string, Diagnostic.t) result =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        let buf = really_input_string ic len in
        Ok buf)
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

let print_help () =
  print_endline "Vesper Compiler & Language Runtime";
  print_endline "";
  print_endline "Usage:";
  print_endline "  vesper <file.vesper>       Parse and validate source file";
  print_endline "  vesper -e <source>         Parse and validate inline source string";
  print_endline "  vesper --help              Display this help message";
  print_endline "  vesper --version           Display version information"

let run_cli argv : int =
  try
    let argc = Array.length argv in
    if argc <= 1 then begin
      print_help ();
      0
    end
    else
      match argv.(1) with
      | "--help" | "-h" ->
          print_help ();
          0
      | "--version" | "-v" ->
          print_endline "vesper 0.1.0 (Phase 2: Diagnostics & Host Isolation)";
          0
      | "-e" when argc > 2 -> (
          let code = argv.(2) in
          match compile_string ~file:"<cli>" code with
          | Ok prog ->
              Printf.printf "Successfully parsed %d declaration(s).\n"
                (List.length prog.decls);
              0
          | Error diag ->
              prerr_endline (Diagnostic.render_terminal ~use_color:true ~source:code diag);
              1)
      | file -> (
          match compile_file file with
          | Ok prog ->
              Printf.printf "Successfully parsed %d declaration(s) in %s.\n"
                (List.length prog.decls) file;
              0
          | Error diag ->
              let source =
                match read_file file with Ok src -> Some src | Error _ -> None
              in
              prerr_endline (Diagnostic.render_terminal ~use_color:true ?source diag);
              1)
  with exn ->
    let diag =
      Diagnostic.error ~code:Error_code.E0002_internal_error ~span:Ast.dummy_span
        (Printf.sprintf "Unhandled host exception: %s" (Printexc.to_string exn))
    in
    prerr_endline (Diagnostic.render_terminal ~use_color:false diag);
    1
