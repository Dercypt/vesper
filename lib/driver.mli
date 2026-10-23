(** Compiler driver orchestrating file I/O, parsing, AST validation, and process
    isolation. Guarantees Law 1 (Total Diagnosability & Host Isolation): no user input or
    runtime exception leaks past this boundary. *)

val read_file : string -> (string, Diagnostic.t) result
(** Safely reads file contents, converting any [Sys_error] or [End_of_file] into a
    structured diagnostic carrying error code [E0001_io_error]. *)

val compile_string : ?file:string -> string -> (Ast.program, Diagnostic.t) result
(** Safely parses and validates a Vesper source string. Guarantees zero uncaught
    exceptions. *)

val compile_file : string -> (Ast.program, Diagnostic.t) result
(** Safely reads, parses, and validates a source file. *)
