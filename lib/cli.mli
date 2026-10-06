(** Production command-line interface for Vesper. Subcommand names and flags are public
    API. Malformed arguments never raise: they produce a usage error (exit code 2). *)

type command =
  | Run of string  (** [vesper run <file>] *)
  | Check of string  (** [vesper check <file>] *)
  | Fmt of { file : string; write : bool }
      (** [vesper fmt [--write] <file>]: pretty-print to stdout, or rewrite in place. *)
  | Repl  (** [vesper repl] *)
  | Disasm of string  (** [vesper disasm <file>] *)
  | Lsp  (** [vesper lsp]: serve the Language Server Protocol over stdin/stdout. *)
  | Help
  | Version
  | Legacy of string array
      (** Backward-compatible forms: [vesper <file>] and [vesper -e <src>]. Carries the
          original argv. *)

val parse_args : string array -> (command, string) result
(** Parses a full [argv] (including program name). Returns a usage message on malformed
    input. *)

val usage : string
(** Help text. *)

val version : string
(** Version string printed by [--version]. *)

val format_source : file:string -> string -> (string, Diagnostic.t) result
(** Losslessly parses and pretty-prints source text with [Printer.to_string]. *)

val execute : command -> int
(** Executes a command against the real process streams. Returns the exit code: 0 on
    success, 1 on diagnostics, 2 on usage errors. *)

val main : string array -> int
(** Full entry point: parses arguments and executes. Catches every host exception (Law 1).
*)
