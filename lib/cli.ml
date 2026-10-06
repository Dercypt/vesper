let version = "vesper 0.1.0"

let usage =
  String.concat "\n"
    [
      "Vesper programming language";
      "";
      "Usage:";
      "  vesper run <file>            Typecheck, compile to bytecode, and execute";
      "  vesper check <file>          Parse and typecheck without executing";
      "  vesper fmt [--write] <file>  Pretty-print source (stdout, or in place with \
       --write)";
      "  vesper repl                  Start an interactive session";
      "  vesper disasm <file>         Print disassembled bytecode";
      "  vesper lsp                   Serve the Language Server Protocol on stdin/stdout";
      "  vesper <file>                Parse and validate a source file";
      "  vesper -e <source>           Parse and validate inline source";
      "  vesper --help                Display this help message";
      "  vesper --version             Display version information";
      "";
      "Exit codes: 0 success, 1 diagnostics, 2 usage error";
      "";
    ]

type command =
  | Run of string
  | Check of string
  | Fmt of { file : string; write : bool }
  | Repl
  | Disasm of string
  | Lsp
  | Help
  | Version
  | Legacy of string array

let parse_args argv =
  let args = Array.to_list argv in
  match args with
  | [] | [ _ ] -> Ok Help
  | _ :: rest -> (
      let one name mk = function
        | [ f ] when String.length f = 0 || f.[0] <> '-' -> Ok (mk f)
        | [] -> Error (Printf.sprintf "'%s' requires a file argument" name)
        | [ f ] -> Error (Printf.sprintf "unknown option '%s' for '%s'" f name)
        | _ -> Error (Printf.sprintf "'%s' takes exactly one file argument" name)
      in
      let none name cmd = function
        | [] -> Ok cmd
        | _ -> Error (Printf.sprintf "'%s' takes no arguments" name)
      in
      match rest with
      | ("--help" | "-h") :: _ -> Ok Help
      | ("--version" | "-v") :: _ -> Ok Version
      | "run" :: tl -> one "run" (fun f -> Run f) tl
      | "check" :: tl -> one "check" (fun f -> Check f) tl
      | "disasm" :: tl -> one "disasm" (fun f -> Disasm f) tl
      | "repl" :: tl -> none "repl" Repl tl
      | "lsp" :: tl -> none "lsp" Lsp tl
      | "fmt" :: tl -> (
          let write, files = List.partition (fun a -> a = "--write" || a = "-w") tl in
          let write = write <> [] in
          match files with
          | [ f ] when String.length f = 0 || f.[0] <> '-' -> Ok (Fmt { file = f; write })
          | [] -> Error "'fmt' requires a file argument"
          | [ f ] -> Error (Printf.sprintf "unknown option '%s' for 'fmt'" f)
          | _ -> Error "'fmt' takes exactly one file argument")
      | "-e" :: _ :: _ -> Ok (Legacy argv)
      | "-e" :: [] -> Error "'-e' requires a source argument"
      | f :: _ when String.length f > 0 && f.[0] = '-' ->
          Error (Printf.sprintf "unknown option '%s'" f)
      | _ -> Ok (Legacy argv))

let format_source ~file src =
  match Parse_facade.parse_string ~file src with
  | Error d -> Error d
  | Ok prog -> Ok (Printer.to_string prog)

let report ?source d =
  prerr_string (Diagnostic.render_terminal ~use_color:false ?source d ^ "\n")

let report_all ?source ds = List.iter (report ?source) ds

let load file =
  match Driver.read_file file with
  | Error d ->
      report d;
      Error 1
  | Ok src -> (
      match Driver.compile_string ~file src with
      | Error d ->
          report ~source:src d;
          Error 1
      | Ok prog -> Ok (src, prog))

let typecheck ~source prog =
  match Typecheck.typecheck_program ~env:(Prelude.default_type_env ()) prog with
  | Ok _ -> Ok ()
  | Error ds ->
      report_all ~source ds;
      Error 1

let cmd_check file =
  match load file with
  | Error c -> c
  | Ok (source, prog) -> (
      match typecheck ~source prog with
      | Error c -> c
      | Ok () ->
          Printf.printf "%s: ok (%d declaration(s))\n" file (List.length prog.decls);
          0)

let compile_with_prelude ~source prog =
  match Compiler.compile (Prelude.with_prelude prog) with
  | Ok chunk -> Ok chunk
  | Error e ->
      report ~source (Diagnostic.of_ast_error e);
      Error 1

let cmd_run file =
  match load file with
  | Error c -> c
  | Ok (source, prog) -> (
      match typecheck ~source prog with
      | Error c -> c
      | Ok () -> (
          match compile_with_prelude ~source prog with
          | Error c -> c
          | Ok chunk -> (
              match Vm.run chunk with
              | Ok Value.VUnit -> 0
              | Ok v ->
                  print_endline (Value.to_string v);
                  0
              | Error d ->
                  report ~source d;
                  1)))

let cmd_disasm file =
  match load file with
  | Error c -> c
  | Ok (source, prog) -> (
      match Compiler.compile prog with
      | Ok chunk ->
          print_string (Disasm.to_string ~name:file chunk);
          0
      | Error e ->
          report ~source (Diagnostic.of_ast_error e);
          1)

let write_file path contents =
  try
    let oc = open_out_bin path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc contents;
        Ok ())
  with Sys_error msg ->
    Error
      (Diagnostic.error ~code:Error_code.E0001_io_error ~span:Ast.dummy_span
         (Printf.sprintf "Cannot write file %S: %s" path msg))

let cmd_fmt file write =
  match Driver.read_file file with
  | Error d ->
      report d;
      1
  | Ok src -> (
      match format_source ~file src with
      | Error d ->
          report ~source:src d;
          1
      | Ok out ->
          if write then (
            match write_file file out with
            | Ok () -> 0
            | Error d ->
                report d;
                1)
          else begin
            print_string out;
            0
          end)

let cmd_repl () =
  Repl.run
    ~read_line:(fun () -> try Some (input_line stdin) with End_of_file -> None)
    ~out:(fun s ->
      print_string s;
      flush stdout)
    ~err:(fun s ->
      prerr_string s;
      flush stderr)

let execute = function
  | Help ->
      print_string usage;
      0
  | Version ->
      print_endline version;
      0
  | Run f -> cmd_run f
  | Check f -> cmd_check f
  | Fmt { file; write } -> cmd_fmt file write
  | Repl -> cmd_repl ()
  | Disasm f -> cmd_disasm f
  | Lsp -> Lsp_server.run stdin stdout
  | Legacy argv -> Driver.run_cli argv

let main argv =
  try
    match parse_args argv with
    | Ok cmd -> execute cmd
    | Error msg ->
        prerr_string (Printf.sprintf "vesper: %s\nTry 'vesper --help' for usage.\n" msg);
        2
  with exn ->
    let diag =
      Diagnostic.error ~code:Error_code.E0002_internal_error ~span:Ast.dummy_span
        (Printf.sprintf "Unhandled host exception: %s" (Printexc.to_string exn))
    in
    report diag;
    1
