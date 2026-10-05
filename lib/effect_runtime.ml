type io_env = {
  stdin : in_channel;
  stdout : Buffer.t;
  stderr : Buffer.t;
  fs_root : string option;
  vfs : (string, string) Hashtbl.t;
}

let create_env ?stdin ?stdin_str ?(stdout = Buffer.create 256)
    ?(stderr = Buffer.create 256) ?fs_root ?(vfs = []) () =
  let vfs_tbl = Hashtbl.create 16 in
  List.iter (fun (k, v) -> Hashtbl.replace vfs_tbl k v) vfs;
  let in_ch =
    match (stdin, stdin_str) with
    | Some ic, _ -> ic
    | None, Some s ->
        let temp_path, oc = Filename.open_temp_file "vesper_in_" ".tmp" in
        output_string oc s;
        close_out oc;
        let ic = open_in_bin temp_path in
        (try Sys.remove temp_path with _ -> ());
        ic
    | None, None -> (
        try open_in "/dev/null"
        with _ ->
          let temp_path, oc = Filename.open_temp_file "vesper_empty_" ".tmp" in
          close_out oc;
          let ic = open_in_bin temp_path in
          (try Sys.remove temp_path with _ -> ());
          ic)
  in
  { stdin = in_ch; stdout; stderr; fs_root; vfs = vfs_tbl }

let print_string env s = Buffer.add_string env.stdout s

let print_endline env s =
  Buffer.add_string env.stdout s;
  Buffer.add_char env.stdout '\n'

let print_int env n = Buffer.add_string env.stdout (string_of_int n)
let prerr_string env s = Buffer.add_string env.stderr s

let prerr_endline env s =
  Buffer.add_string env.stderr s;
  Buffer.add_char env.stderr '\n'

let get_stdout env = Buffer.contents env.stdout
let get_stderr env = Buffer.contents env.stderr

let clear_buffers env =
  Buffer.clear env.stdout;
  Buffer.clear env.stderr

let read_line ?(span = Ast.dummy_span) env =
  try Ok (input_line env.stdin) with
  | End_of_file ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span "End of file on stdin")
  | Sys_error msg ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "I/O error reading stdin: %s" msg))
  | exn ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "Host exception reading stdin: %s" (Printexc.to_string exn)))

let read_all ?(span = Ast.dummy_span) env =
  try
    let buf = Buffer.create 512 in
    let chunk = Bytes.create 1024 in
    let rec loop () =
      let n = input env.stdin chunk 0 1024 in
      if n = 0 then Ok (Buffer.contents buf)
      else begin
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
      end
    in
    loop ()
  with
  | Sys_error msg ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "I/O error reading stdin: %s" msg))
  | exn ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           (Printf.sprintf "Host exception reading stdin: %s" (Printexc.to_string exn)))

let split_path (path : string) : string list =
  let len = String.length path in
  let rec loop start idx acc =
    if idx >= len then
      if idx > start then
        let seg = String.sub path start (idx - start) in
        List.rev (seg :: acc)
      else List.rev acc
    else if String.get path idx = '/' || String.get path idx = '\\' then
      if idx > start then
        let seg = String.sub path start (idx - start) in
        loop (idx + 1) (idx + 1) (seg :: acc)
      else loop (idx + 1) (idx + 1) acc
    else loop start (idx + 1) acc
  in
  loop 0 0 []

let sanitize_path ?(span = Ast.dummy_span) ~root (path : string) :
    (string, Diagnostic.t) result =
  match root with
  | None ->
      Error
        (Diagnostic.error ~code:Error_code.E0001_io_error ~span
           "File system access denied: no fs_root capability configured")
  | Some root_dir ->
      let is_abs =
        String.length path > 0 && (String.get path 0 = '/' || String.get path 0 = '\\')
      in
      let root_parts = split_path root_dir in
      let path_parts = split_path path in
      if is_abs then
        (* Absolute path: must have root_parts as a prefix, or reject *)
        let rec check_prefix rp pp =
          match (rp, pp) with
          | [], remaining ->
              let rec resolve acc = function
                | [] -> Ok (String.concat "/" (root_parts @ List.rev acc))
                | "." :: xs -> resolve acc xs
                | ".." :: _ when acc = [] ->
                    Error
                      (Diagnostic.error ~code:Error_code.E0001_io_error ~span
                         (Printf.sprintf
                            "Security violation: path '%s' escapes sandbox root '%s'" path
                            root_dir))
                | ".." :: xs -> resolve (List.tl acc) xs
                | x :: xs -> resolve (x :: acc) xs
              in
              resolve [] remaining
          | r :: rs, p :: ps when String.equal r p -> check_prefix rs ps
          | _ ->
              Error
                (Diagnostic.error ~code:Error_code.E0001_io_error ~span
                   (Printf.sprintf
                      "Security violation: absolute path '%s' escapes sandbox root '%s'"
                      path root_dir))
        in
        check_prefix root_parts path_parts
      else
        (* Relative path: resolve against root_parts *)
        let rec resolve acc = function
          | [] -> Ok (String.concat "/" (root_parts @ List.rev acc))
          | "." :: xs -> resolve acc xs
          | ".." :: _ when acc = [] ->
              Error
                (Diagnostic.error ~code:Error_code.E0001_io_error ~span
                   (Printf.sprintf
                      "Security violation: directory traversal attack in path '%s'" path))
          | ".." :: xs -> resolve (List.tl acc) xs
          | x :: xs -> resolve (x :: acc) xs
        in
        resolve [] path_parts

let read_file ?(span = Ast.dummy_span) env path =
  (* Check in-memory VFS first *)
  if Hashtbl.mem env.vfs path then Ok (Hashtbl.find env.vfs path)
  else
    match sanitize_path ~span ~root:env.fs_root path with
    | Error diag -> Error diag
    | Ok sanitized -> (
        if Hashtbl.mem env.vfs sanitized then Ok (Hashtbl.find env.vfs sanitized)
        else
          try
            let ic = open_in_bin sanitized in
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () ->
                let len = in_channel_length ic in
                Ok (really_input_string ic len))
          with
          | Sys_error msg ->
              Error
                (Diagnostic.error ~code:Error_code.E0001_io_error ~span
                   (Printf.sprintf "Cannot read file '%s': %s" path msg))
          | exn ->
              Error
                (Diagnostic.error ~code:Error_code.E0001_io_error ~span
                   (Printf.sprintf "Failed reading file '%s': %s" path
                      (Printexc.to_string exn))))

let write_file ?(span = Ast.dummy_span) env path content =
  match sanitize_path ~span ~root:env.fs_root path with
  | Error diag -> Error diag
  | Ok sanitized ->
      Hashtbl.replace env.vfs path content;
      Hashtbl.replace env.vfs sanitized content;
      (* If physical directory exists, try to persist safely *)
      (try
         let oc = open_out_bin sanitized in
         Fun.protect
           ~finally:(fun () -> close_out_noerr oc)
           (fun () -> output_string oc content)
       with _ -> ());
      Ok ()

let file_exists env path =
  if Hashtbl.mem env.vfs path then true
  else
    match sanitize_path ~root:env.fs_root path with
    | Error _ -> false
    | Ok sanitized -> (
        Hashtbl.mem env.vfs sanitized || try Sys.file_exists sanitized with _ -> false)

let delete_file ?(span = Ast.dummy_span) env path =
  let was_in_vfs = Hashtbl.mem env.vfs path in
  Hashtbl.remove env.vfs path;
  match sanitize_path ~span ~root:env.fs_root path with
  | Error diag -> if was_in_vfs then Ok () else Error diag
  | Ok sanitized ->
      Hashtbl.remove env.vfs sanitized;
      (try if Sys.file_exists sanitized then Sys.remove sanitized with _ -> ());
      Ok ()

let list_files ?(span = Ast.dummy_span) env =
  ignore span;
  let items = Hashtbl.fold (fun k _ acc -> k :: acc) env.vfs [] in
  Ok (List.sort String.compare items)
