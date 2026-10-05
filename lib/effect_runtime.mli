(** Sandboxed capability-based effect runtime and host boundary. Adheres to Law 1 (Total
    Diagnosability & Host Isolation) and Law 5 (Deterministic Evaluation): ambient I/O is
    strictly forbidden; all side effects are memory-buffered, mockable, and bound to
    explicit capabilities. *)

type io_env = {
  stdin : in_channel;
  stdout : Buffer.t;
  stderr : Buffer.t;
  fs_root : string option;
  vfs : (string, string) Hashtbl.t;
}
(** Capability environment token bundling isolated console streams and sandboxed
    filesystem. *)

val create_env :
  ?stdin:in_channel ->
  ?stdin_str:string ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  ?fs_root:string ->
  ?vfs:(string * string) list ->
  unit ->
  io_env
(** Constructs an isolated I/O environment. If [stdin_str] is provided, creates a mock
    input channel reading from the string. If [vfs] is provided, seeds the virtual
    filesystem. *)

val print_string : io_env -> string -> unit
(** Emits a string to the environment's isolated stdout buffer. *)

val print_endline : io_env -> string -> unit
(** Emits a string followed by a newline to the environment's isolated stdout buffer. *)

val print_int : io_env -> int -> unit
(** Emits an integer to the environment's isolated stdout buffer. *)

val prerr_string : io_env -> string -> unit
(** Emits a string to the environment's isolated stderr buffer. *)

val prerr_endline : io_env -> string -> unit
(** Emits a string followed by a newline to the environment's isolated stderr buffer. *)

val get_stdout : io_env -> string
(** Returns accumulated content of the environment's stdout buffer. *)

val get_stderr : io_env -> string
(** Returns accumulated content of the environment's stderr buffer. *)

val clear_buffers : io_env -> unit
(** Resets both stdout and stderr buffers to empty. *)

val read_line : ?span:Ast.span -> io_env -> (string, Diagnostic.t) result
(** Reads a single line from the environment's stdin channel without throwing host
    exceptions. *)

val read_all : ?span:Ast.span -> io_env -> (string, Diagnostic.t) result
(** Reads all remaining bytes from the environment's stdin channel. *)

val sanitize_path :
  ?span:Ast.span -> root:string option -> string -> (string, Diagnostic.t) result
(** Validates and normalizes a filesystem path against a designated sandbox root. Safely
    rejects directory traversal attacks (such as [..] escapes or unauthorized absolute
    paths) with a structured [E0001_io_error] diagnostic. *)

val read_file : ?span:Ast.span -> io_env -> string -> (string, Diagnostic.t) result
(** Safely reads a file from the virtual filesystem or sandboxed physical root. Rejects
    paths escaping [fs_root] or unauthorized accesses. *)

val write_file :
  ?span:Ast.span -> io_env -> string -> string -> (unit, Diagnostic.t) result
(** Safely writes content to a file in the virtual filesystem or sandboxed physical root.
    Rejects directory traversal attacks. *)

val file_exists : io_env -> string -> bool
(** Checks whether a file exists in the virtual filesystem or sandboxed root. *)

val delete_file : ?span:Ast.span -> io_env -> string -> (unit, Diagnostic.t) result
(** Deletes a file from the virtual filesystem or sandboxed root. *)

val list_files : ?span:Ast.span -> io_env -> (string list, Diagnostic.t) result
(** Lists all virtual files registered in the environment's virtual filesystem. *)
