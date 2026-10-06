(** Language Server Protocol skeleton: JSON-RPC 2.0 dispatch over stdin/stdout with
    [Content-Length] framing. Supports document sync, diagnostics publication, and
    whole-document formatting. Never raises on malformed client input (Law 1). *)

type t
(** Immutable server state (open documents and lifecycle flags). *)

val initial : t
(** State before the [initialize] handshake. *)

val diagnostics : file:string -> string -> Diagnostic.t list
(** Parses and typechecks source text (with the prelude environment), returning every
    diagnostic found. Empty list means the document is clean. *)

val handle_message : t -> Json.t -> t * Json.t list
(** Dispatches one decoded JSON-RPC message. Returns the new state and the outgoing
    messages (responses and notifications) in order. Supported methods: [initialize],
    [initialized], [shutdown], [exit], [textDocument/didOpen], [textDocument/didChange],
    [textDocument/didClose], [textDocument/formatting]. Unknown requests get a [-32601]
    error; unknown notifications are ignored. *)

val exit_code : t -> int option
(** [Some code] once an [exit] notification was received: 0 if [shutdown] preceded it,
    else 1. *)

val run : in_channel -> out_channel -> int
(** Serves the protocol until [exit] or end of input; returns the process exit code. *)
