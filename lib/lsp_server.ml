type t = { docs : (string * string) list; shutdown : bool; exit : int option }

let initial = { docs = []; shutdown = false; exit = None }
let exit_code st = st.exit

let diagnostics ~file src =
  try
    match Parse_facade.parse_string ~file src with
    | Error d -> [ d ]
    | Ok prog -> (
        match Typecheck.typecheck_program ~env:(Prelude.default_type_env ()) prog with
        | Ok _ -> []
        | Error ds -> ds)
  with exn ->
    [
      Diagnostic.error ~code:Error_code.E0002_internal_error ~span:Ast.dummy_span
        ("Unhandled host exception: " ^ Printexc.to_string exn);
    ]

let position line col =
  Json.Object [ ("line", Json.Int (max 0 line)); ("character", Json.Int (max 0 col)) ]

let lsp_severity = function
  | Diagnostic.Error -> 1
  | Diagnostic.Warning -> 2
  | Diagnostic.Note -> 3

let diagnostic_json (d : Diagnostic.t) =
  let s = d.span in
  let message =
    match d.hint with None -> d.message | Some h -> d.message ^ "\nhint: " ^ h
  in
  Json.Object
    [
      ( "range",
        Json.Object
          [
            ("start", position (s.start_line - 1) s.start_col);
            ("end", position (s.end_line - 1) s.end_col);
          ] );
      ("severity", Json.Int (lsp_severity d.severity));
      ("code", Json.String (Error_code.to_code_string d.code));
      ("source", Json.String "vesper");
      ("message", Json.String message);
    ]

let response id result =
  Json.Object [ ("jsonrpc", Json.String "2.0"); ("id", id); ("result", result) ]

let error_response id code message =
  Json.Object
    [
      ("jsonrpc", Json.String "2.0");
      ("id", id);
      ("error", Json.Object [ ("code", Json.Int code); ("message", Json.String message) ]);
    ]

let notification meth params =
  Json.Object
    [ ("jsonrpc", Json.String "2.0"); ("method", Json.String meth); ("params", params) ]

let publish uri src =
  notification "textDocument/publishDiagnostics"
    (Json.Object
       [
         ("uri", Json.String uri);
         ("diagnostics", Json.List (List.map diagnostic_json (diagnostics ~file:uri src)));
       ])

let clear uri =
  notification "textDocument/publishDiagnostics"
    (Json.Object [ ("uri", Json.String uri); ("diagnostics", Json.List []) ])

let set_doc docs uri text = (uri, text) :: List.remove_assoc uri docs
let ( >>= ) o f = match o with Some x -> f x | None -> None

let doc_uri params =
  Json.member "textDocument" params >>= Json.member "uri" >>= Json.to_string_opt

let end_position text =
  let lines = ref 0 and last = ref 0 in
  String.iter
    (fun c ->
      if c = '\n' then begin
        incr lines;
        last := 0
      end
      else incr last)
    text;
  position !lines !last

let capabilities =
  Json.Object
    [
      ( "capabilities",
        Json.Object
          [
            ("textDocumentSync", Json.Int 1);
            ("documentFormattingProvider", Json.Bool true);
          ] );
      ( "serverInfo",
        Json.Object [ ("name", Json.String "vesper"); ("version", Json.String "0.1.0") ]
      );
    ]

let invalid_params id = error_response id (-32602) "Invalid params"

let handle_request st id meth params =
  match meth with
  | "initialize" -> (st, [ response id capabilities ])
  | "shutdown" -> ({ st with shutdown = true }, [ response id Json.Null ])
  | "textDocument/formatting" -> (
      match doc_uri params with
      | None -> (st, [ invalid_params id ])
      | Some uri -> (
          match List.assoc_opt uri st.docs with
          | None -> (st, [ error_response id (-32602) "Document is not open" ])
          | Some text -> (
              match Parse_facade.parse_string ~file:uri text with
              | Error d ->
                  (st, [ error_response id (-32803) ("Cannot format: " ^ d.message) ])
              | Ok prog ->
                  let formatted = Printer.to_string prog in
                  let edit =
                    Json.Object
                      [
                        ( "range",
                          Json.Object
                            [ ("start", position 0 0); ("end", end_position text) ] );
                        ("newText", Json.String formatted);
                      ]
                  in
                  (st, [ response id (Json.List [ edit ]) ]))))
  | _ -> (st, [ error_response id (-32601) ("Method not found: " ^ meth) ])

let handle_notification st meth params =
  match meth with
  | "textDocument/didOpen" -> (
      let item = Json.member "textDocument" params in
      let uri = item >>= Json.member "uri" >>= Json.to_string_opt in
      let text = item >>= Json.member "text" >>= Json.to_string_opt in
      match (uri, text) with
      | Some uri, Some text ->
          ({ st with docs = set_doc st.docs uri text }, [ publish uri text ])
      | _ -> (st, []))
  | "textDocument/didChange" -> (
      let uri = doc_uri params in
      let changes = Json.member "contentChanges" params >>= Json.to_list_opt in
      let last_text =
        match changes with
        | Some (_ :: _ as l) ->
            Json.member "text" (List.nth l (List.length l - 1)) >>= Json.to_string_opt
        | _ -> None
      in
      match (uri, last_text) with
      | Some uri, Some text ->
          ({ st with docs = set_doc st.docs uri text }, [ publish uri text ])
      | _ -> (st, []))
  | "textDocument/didClose" -> (
      match doc_uri params with
      | Some uri -> ({ st with docs = List.remove_assoc uri st.docs }, [ clear uri ])
      | None -> (st, []))
  | "exit" -> ({ st with exit = Some (if st.shutdown then 0 else 1) }, [])
  | _ -> (st, [])

let handle_message st msg =
  match (Json.member "method" msg >>= Json.to_string_opt, Json.member "id" msg) with
  | Some meth, Some id ->
      let params = Option.value (Json.member "params" msg) ~default:Json.Null in
      handle_request st id meth params
  | Some meth, None ->
      let params = Option.value (Json.member "params" msg) ~default:Json.Null in
      handle_notification st meth params
  | None, _ -> (
      match Json.member "id" msg with
      | Some id -> (st, [ error_response id (-32600) "Invalid Request" ])
      | None -> (st, []))

(* ---- Transport ---- *)

let read_headers ic =
  let rec loop len =
    match input_line ic with
    | exception End_of_file -> None
    | line ->
        let line = String.trim line in
        if line = "" then Some len
        else
          let len =
            match String.index_opt line ':' with
            | Some i when String.lowercase_ascii (String.sub line 0 i) = "content-length"
              ->
                int_of_string_opt
                  (String.trim (String.sub line (i + 1) (String.length line - i - 1)))
            | _ -> len
          in
          loop len
  in
  loop None

let read_message ic =
  match read_headers ic with
  | None -> `Eof
  | Some None -> `Bad "Missing Content-Length header"
  | Some (Some n) when n < 0 -> `Bad "Negative Content-Length"
  | Some (Some n) -> (
      match really_input_string ic n with
      | exception End_of_file -> `Eof
      | body -> ( match Json.parse body with Ok j -> `Msg j | Error e -> `Bad e))

let send oc msg =
  let body = Json.to_string msg in
  output_string oc
    (Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body);
  flush oc

let run ic oc =
  let rec loop st =
    match exit_code st with
    | Some code -> code
    | None -> (
        match read_message ic with
        | `Eof -> if st.shutdown then 0 else 1
        | `Bad e ->
            send oc (error_response Json.Null (-32700) ("Parse error: " ^ e));
            loop st
        | `Msg m ->
            let st', out =
              try handle_message st m
              with exn ->
                ( st,
                  [
                    error_response
                      (Option.value (Json.member "id" m) ~default:Json.Null)
                      (-32603)
                      ("Internal error: " ^ Printexc.to_string exn);
                  ] )
            in
            List.iter (send oc) out;
            loop st')
  in
  try loop initial with Sys_error _ -> 1
