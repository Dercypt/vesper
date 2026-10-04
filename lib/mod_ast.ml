type path_segment = { name : string; span : Ast.span }
type path = { span : Ast.span; segments : path_segment list }

type type_spec = {
  name : string;
  params : string list;
  manifest : Types.t option;
  span : Ast.span;
}

type val_spec = { name : string; ty : Types.t; span : Ast.span }

type sig_item =
  | SigVal of val_spec
  | SigType of type_spec
  | SigModule of { name : string; signature : mod_sig; span : Ast.span }
  | SigOpen of { path : path; span : Ast.span }

and mod_sig =
  | SigBody of { items : sig_item list; span : Ast.span }
  | SigIdent of { path : path; span : Ast.span }

type val_def = {
  name : string;
  is_rec : bool;
  args : string list;
  value : Ast.expr;
  annot : Types.t option;
  span : Ast.span;
}

type type_def = { name : string; params : string list; ty : Types.t; span : Ast.span }

type mod_item =
  | ModVal of val_def
  | ModType of type_def
  | ModModule of { name : string; expr : mod_expr; span : Ast.span }
  | ModOpen of { path : path; span : Ast.span }
  | ModImport of { path : path; alias : string option; span : Ast.span }

and mod_expr =
  | ModStruct of { items : mod_item list; span : Ast.span }
  | ModVar of { path : path; span : Ast.span }
  | ModAscribed of { expr : mod_expr; signature : mod_sig; span : Ast.span }

type unit_kind = Implementation of mod_expr | Interface of mod_sig

type compilation_unit = {
  name : string;
  file_path : string;
  kind : unit_kind;
  span : Ast.span;
}

(** {1 Smart Constructors} *)

let make_segment ~span name = { name; span }
let make_path ~span segments = { span; segments }

let parse_path ~span str =
  let trimmed = String.trim str in
  if String.length trimmed = 0 then
    Error { Ast.span; message = "Cannot parse empty path identifier" }
  else
    let parts = String.split_on_char '.' trimmed in
    let rec build_segments col_offset = function
      | [] -> Ok []
      | "" :: _ ->
          Error
            {
              Ast.span;
              message = Printf.sprintf "Invalid empty segment in path '%s'" trimmed;
            }
      | seg :: rest -> (
          let seg_len = String.length seg in
          let seg_span =
            match
              Ast.create_span ~file:span.file ~start_line:span.start_line
                ~start_col:col_offset ~end_line:span.start_line
                ~end_col:(col_offset + seg_len)
            with
            | Ok s -> s
            | Error _ -> span
          in
          let seg_item = { name = seg; span = seg_span } in
          match build_segments (col_offset + seg_len + 1) rest with
          | Ok next_segs -> Ok (seg_item :: next_segs)
          | Error err -> Error err)
    in
    match build_segments span.start_col parts with
    | Ok segs -> Ok { span; segments = segs }
    | Error err -> Error err

let path_of_strings ~span string_list =
  if string_list = [] then
    Error { Ast.span; message = "Cannot create empty path from empty list" }
  else
    let str = String.concat "." string_list in
    parse_path ~span str

let make_val_spec ~span ~name ~ty = { name; ty; span }

let make_type_spec ~span ~name ?(params = []) ?manifest () =
  { name; params; manifest; span }

let make_sig_val ~span ~name ~ty = SigVal (make_val_spec ~span ~name ~ty)

let make_sig_type ~span ~name ?params ?manifest () =
  SigType (make_type_spec ~span ~name ?params ?manifest ())

let make_sig_module ~span ~name signature = SigModule { name; signature; span }
let make_sig_open ~span path = SigOpen { path; span }
let make_sig_body ~span items = SigBody { items; span }
let make_sig_ident ~span path = SigIdent { path; span }

let make_val_def ~span ~name ?(is_rec = false) ?(args = []) ?annot value =
  { name; is_rec; args; value; annot; span }

let make_type_def ~span ~name ?(params = []) ~ty () = { name; params; ty; span }

let make_mod_val ~span ~name ?is_rec ?args ?annot value =
  ModVal (make_val_def ~span ~name ?is_rec ?args ?annot value)

let make_mod_type ~span ~name ?params ~ty () =
  ModType (make_type_def ~span ~name ?params ~ty ())

let make_mod_module ~span ~name expr = ModModule { name; expr; span }
let make_mod_open ~span path = ModOpen { path; span }
let make_mod_import ~span ?alias path = ModImport { path; alias; span }
let make_mod_struct ~span items = ModStruct { items; span }
let make_mod_var ~span path = ModVar { path; span }
let make_mod_ascribed ~span expr signature = ModAscribed { expr; signature; span }
let make_unit ~span ~name ~file_path kind = { name; file_path; kind; span }

(** {1 Path Operations} *)

let path_to_string (path : path) =
  String.concat "." (List.map (fun (s : path_segment) -> s.name) path.segments)

let head_segment (path : path) =
  match path.segments with
  | [] -> Error { Ast.span = path.span; message = "Empty path has no head segment" }
  | s :: _ -> Ok s

let last_segment (path : path) =
  match List.rev path.segments with
  | [] -> Error { Ast.span = path.span; message = "Empty path has no last segment" }
  | s :: _ -> Ok s

let drop_last (path : path) =
  match List.rev path.segments with
  | [] -> Error { Ast.span = path.span; message = "Cannot drop from empty path" }
  | last :: rev_prefix ->
      let prefix_segs = List.rev rev_prefix in
      let prefix_span =
        match prefix_segs with
        | [] -> path.span
        | first :: _ -> (
            let last_p = List.hd (List.rev prefix_segs) in
            match
              Ast.create_span ~file:path.span.file ~start_line:first.span.start_line
                ~start_col:first.span.start_col ~end_line:last_p.span.end_line
                ~end_col:last_p.span.end_col
            with
            | Ok s -> s
            | Error _ -> path.span)
      in
      Ok ({ span = prefix_span; segments = prefix_segs }, last)

let append_segment (path : path) (segment : path_segment) =
  let new_span =
    match
      Ast.create_span ~file:path.span.file ~start_line:path.span.start_line
        ~start_col:path.span.start_col ~end_line:segment.span.end_line
        ~end_col:segment.span.end_col
    with
    | Ok s -> s
    | Error _ -> path.span
  in
  { span = new_span; segments = path.segments @ [ segment ] }

(** {1 Validation Functions (Law 3: Strict Source Provenance)} *)

let validate_path (path : path) =
  if not (Ast.span_is_valid path.span) then
    Error { Ast.span = path.span; message = "Path contains invalid span coordinates" }
  else if path.segments = [] then
    Error { Ast.span = path.span; message = "Path cannot have zero segments" }
  else
    let rec check_segs = function
      | [] -> Ok ()
      | (seg : path_segment) :: rest ->
          if not (Ast.span_is_valid seg.span) then
            Error
              {
                Ast.span = seg.span;
                message = Printf.sprintf "Path segment '%s' has invalid span" seg.name;
              }
          else if String.length seg.name = 0 then
            Error { Ast.span = seg.span; message = "Path segment name cannot be empty" }
          else check_segs rest
    in
    check_segs path.segments

let rec validate_sig_item = function
  | SigVal spec ->
      if not (Ast.span_is_valid spec.span) then
        Error { Ast.span = spec.span; message = "Invalid span on val signature" }
      else if String.length spec.name = 0 then
        Error { Ast.span = spec.span; message = "Val signature cannot have empty name" }
      else Ok ()
  | SigType spec ->
      if not (Ast.span_is_valid spec.span) then
        Error { Ast.span = spec.span; message = "Invalid span on type signature" }
      else if String.length spec.name = 0 then
        Error { Ast.span = spec.span; message = "Type signature cannot have empty name" }
      else Ok ()
  | SigModule { name; signature; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on module signature" }
      else if String.length name = 0 then
        Error { Ast.span; message = "Module signature cannot have empty name" }
      else validate_sig signature
  | SigOpen { path; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on sig open" }
      else validate_path path

and validate_sig = function
  | SigBody { items; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on signature body" }
      else
        let rec check_items = function
          | [] -> Ok ()
          | it :: rest -> (
              match validate_sig_item it with
              | Ok () -> check_items rest
              | Error err -> Error err)
        in
        check_items items
  | SigIdent { path; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on signature identifier" }
      else validate_path path

let rec validate_mod_item = function
  | ModVal def ->
      if not (Ast.span_is_valid def.span) then
        Error { Ast.span = def.span; message = "Invalid span on val definition" }
      else if String.length def.name = 0 then
        Error { Ast.span = def.span; message = "Val definition cannot have empty name" }
      else Ast.validate_expr def.value
  | ModType def ->
      if not (Ast.span_is_valid def.span) then
        Error { Ast.span = def.span; message = "Invalid span on type definition" }
      else if String.length def.name = 0 then
        Error { Ast.span = def.span; message = "Type definition cannot have empty name" }
      else Ok ()
  | ModModule { name; expr; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on module item" }
      else if String.length name = 0 then
        Error { Ast.span; message = "Module item cannot have empty name" }
      else validate_mod_expr expr
  | ModOpen { path; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on open item" }
      else validate_path path
  | ModImport { path; alias; span } -> (
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on import item" }
      else
        match alias with
        | Some a when String.length a = 0 ->
            Error { Ast.span; message = "Import alias cannot be empty" }
        | _ -> validate_path path)

and validate_mod_expr = function
  | ModStruct { items; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on module structure" }
      else
        let rec check_items = function
          | [] -> Ok ()
          | it :: rest -> (
              match validate_mod_item it with
              | Ok () -> check_items rest
              | Error err -> Error err)
        in
        check_items items
  | ModVar { path; span } ->
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on module variable" }
      else validate_path path
  | ModAscribed { expr; signature; span } -> (
      if not (Ast.span_is_valid span) then
        Error { Ast.span; message = "Invalid span on ascribed module" }
      else
        match validate_mod_expr expr with
        | Ok () -> validate_sig signature
        | Error err -> Error err)

let validate_unit unit =
  if not (Ast.span_is_valid unit.span) then
    Error { Ast.span = unit.span; message = "Invalid span on compilation unit" }
  else if String.length unit.name = 0 then
    Error { Ast.span = unit.span; message = "Compilation unit cannot have empty name" }
  else
    match unit.kind with
    | Implementation expr -> validate_mod_expr expr
    | Interface sign -> validate_sig sign

(** {1 Structural Equality (Modulo Spans)} *)

let path_equal p1 p2 =
  List.length p1.segments = List.length p2.segments
  && List.for_all2
       (fun (s1 : path_segment) (s2 : path_segment) -> s1.name = s2.name)
       p1.segments p2.segments

let rec sig_item_equal i1 i2 =
  match (i1, i2) with
  | SigVal v1, SigVal v2 -> v1.name = v2.name && Types.equal v1.ty v2.ty
  | SigType t1, SigType t2 -> (
      t1.name = t2.name && t1.params = t2.params
      &&
      match (t1.manifest, t2.manifest) with
      | None, None -> true
      | Some m1, Some m2 -> Types.equal m1 m2
      | _ -> false)
  | SigModule m1, SigModule m2 -> m1.name = m2.name && sig_equal m1.signature m2.signature
  | SigOpen o1, SigOpen o2 -> path_equal o1.path o2.path
  | _ -> false

and sig_equal s1 s2 =
  match (s1, s2) with
  | SigBody b1, SigBody b2 ->
      List.length b1.items = List.length b2.items
      && List.for_all2 sig_item_equal b1.items b2.items
  | SigIdent i1, SigIdent i2 -> path_equal i1.path i2.path
  | _ -> false

let rec mod_item_equal m1 m2 =
  match (m1, m2) with
  | ModVal v1, ModVal v2 -> (
      v1.name = v2.name && v1.is_rec = v2.is_rec && v1.args = v2.args
      && Ast.expr_equal v1.value v2.value
      &&
      match (v1.annot, v2.annot) with
      | None, None -> true
      | Some a1, Some a2 -> Types.equal a1 a2
      | _ -> false)
  | ModType t1, ModType t2 ->
      t1.name = t2.name && t1.params = t2.params && Types.equal t1.ty t2.ty
  | ModModule m1, ModModule m2 -> m1.name = m2.name && mod_expr_equal m1.expr m2.expr
  | ModOpen o1, ModOpen o2 -> path_equal o1.path o2.path
  | ModImport i1, ModImport i2 -> path_equal i1.path i2.path && i1.alias = i2.alias
  | _ -> false

and mod_expr_equal e1 e2 =
  match (e1, e2) with
  | ModStruct s1, ModStruct s2 ->
      List.length s1.items = List.length s2.items
      && List.for_all2 mod_item_equal s1.items s2.items
  | ModVar v1, ModVar v2 -> path_equal v1.path v2.path
  | ModAscribed a1, ModAscribed a2 ->
      mod_expr_equal a1.expr a2.expr && sig_equal a1.signature a2.signature
  | _ -> false

let unit_equal u1 u2 =
  u1.name = u2.name && u1.file_path = u2.file_path
  &&
  match (u1.kind, u2.kind) with
  | Implementation e1, Implementation e2 -> mod_expr_equal e1 e2
  | Interface s1, Interface s2 -> sig_equal s1 s2
  | _ -> false

(** {1 Pretty-Printing & Formatting (Law 2: Invertible Syntax)} *)

let pp_path fmt path = Format.pp_print_string fmt (path_to_string path)

let rec pp_sig_item fmt = function
  | SigVal spec -> Format.fprintf fmt "val %s : %s" spec.name (Types.to_string spec.ty)
  | SigType spec -> (
      match spec.manifest with
      | None ->
          if spec.params = [] then Format.fprintf fmt "type %s" spec.name
          else
            Format.fprintf fmt "type (%s) %s" (String.concat ", " spec.params) spec.name
      | Some m ->
          if spec.params = [] then
            Format.fprintf fmt "type %s = %s" spec.name (Types.to_string m)
          else
            Format.fprintf fmt "type (%s) %s = %s"
              (String.concat ", " spec.params)
              spec.name (Types.to_string m))
  | SigModule { name; signature; _ } ->
      Format.fprintf fmt "module %s : %s" name (sig_to_string signature)
  | SigOpen { path; _ } -> Format.fprintf fmt "open %s" (path_to_string path)

and pp_sig fmt = function
  | SigBody { items; _ } ->
      Format.fprintf fmt "sig\n";
      List.iter (fun it -> Format.fprintf fmt "  %a\n" pp_sig_item it) items;
      Format.fprintf fmt "end"
  | SigIdent { path; _ } -> Format.pp_print_string fmt (path_to_string path)

and sig_item_to_string it =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  pp_sig_item fmt it;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

and sig_to_string s =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_sig fmt s;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let rec pp_mod_item fmt = function
  | ModVal def ->
      let rec_str = if def.is_rec then " rec" else "" in
      let args_str = if def.args = [] then "" else " " ^ String.concat " " def.args in
      let annot_str =
        match def.annot with None -> "" | Some t -> " : " ^ Types.to_string t
      in
      Format.fprintf fmt "let%s %s%s%s = %s" rec_str def.name args_str annot_str
        (Printer.expr_to_string def.value)
  | ModType def ->
      if def.params = [] then
        Format.fprintf fmt "type %s = %s" def.name (Types.to_string def.ty)
      else
        Format.fprintf fmt "type (%s) %s = %s"
          (String.concat ", " def.params)
          def.name (Types.to_string def.ty)
  | ModModule { name; expr; _ } ->
      Format.fprintf fmt "module %s = %s" name (mod_expr_to_string expr)
  | ModOpen { path; _ } -> Format.fprintf fmt "open %s" (path_to_string path)
  | ModImport { path; alias; _ } -> (
      match alias with
      | None -> Format.fprintf fmt "import %s" (path_to_string path)
      | Some a -> Format.fprintf fmt "import %s as %s" (path_to_string path) a)

and pp_mod_expr fmt = function
  | ModStruct { items; _ } ->
      Format.fprintf fmt "struct\n";
      List.iter (fun it -> Format.fprintf fmt "  %a\n" pp_mod_item it) items;
      Format.fprintf fmt "end"
  | ModVar { path; _ } -> Format.pp_print_string fmt (path_to_string path)
  | ModAscribed { expr; signature; _ } ->
      Format.fprintf fmt "(%s : %s)" (mod_expr_to_string expr) (sig_to_string signature)

and mod_item_to_string it =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  pp_mod_item fmt it;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

and mod_expr_to_string e =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_mod_expr fmt e;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let pp_unit fmt unit =
  match unit.kind with
  | Implementation expr ->
      Format.fprintf fmt "(* %s *) \n%a" unit.file_path pp_mod_expr expr
  | Interface sign -> Format.fprintf fmt "(* %s *) \n%a" unit.file_path pp_sig sign

let unit_to_string unit =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp_unit fmt unit;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** {1 AST Conversion & File Parsing} *)

let of_ast_decls (decls : Ast.decl list) : mod_item list =
  List.map
    (fun (d : Ast.decl) ->
      match d.decl_desc with
      | Ast.LetDecl { name; is_rec; args; value } ->
          ModVal { name; is_rec; args; value; annot = None; span = d.span }
      | Ast.ExprDecl expr ->
          ModVal
            {
              name = "_";
              is_rec = false;
              args = [];
              value = expr;
              annot = None;
              span = d.span;
            })
    decls

let of_ast_program ?(name = "Main") ?(file_path = "main.vesper") (prog : Ast.program) :
    compilation_unit =
  let items = of_ast_decls prog.decls in
  let mod_expr = ModStruct { items; span = prog.span } in
  { name; file_path; kind = Implementation mod_expr; span = prog.span }

(** Lightweight pure scanner & parser for types in signature files *)
let parse_type_str ~(span : Ast.span) (str : string) : (Types.t, Ast.error) result =
  let tokens =
    let len = String.length str in
    let rec scan idx acc =
      if idx >= len then List.rev acc
      else
        match str.[idx] with
        | ' ' | '\t' | '\r' | '\n' -> scan (idx + 1) acc
        | '-' when idx + 1 < len && str.[idx + 1] = '>' -> scan (idx + 2) ("->" :: acc)
        | '(' -> scan (idx + 1) ("(" :: acc)
        | ')' -> scan (idx + 1) (")" :: acc)
        | '\'' ->
            let start = idx in
            let rec scan_id i =
              if
                i < len
                && ((str.[i] >= 'a' && str.[i] <= 'z')
                   || (str.[i] >= '0' && str.[i] <= '9')
                   || str.[i] = '_')
              then scan_id (i + 1)
              else i
            in
            let end_idx = scan_id (idx + 1) in
            scan end_idx (String.sub str start (end_idx - start) :: acc)
        | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
            let start = idx in
            let rec scan_id i =
              if
                i < len
                && ((str.[i] >= 'a' && str.[i] <= 'z')
                   || (str.[i] >= 'A' && str.[i] <= 'Z')
                   || (str.[i] >= '0' && str.[i] <= '9')
                   || str.[i] = '_')
              then scan_id (i + 1)
              else i
            in
            let end_idx = scan_id (idx + 1) in
            scan end_idx (String.sub str start (end_idx - start) :: acc)
        | _ -> scan (idx + 1) acc
    in
    scan 0 []
  in
  let ty_var_map = ref [] in
  let get_ty_var name =
    match List.assoc_opt name !ty_var_map with
    | Some id -> id
    | None ->
        let id = List.length !ty_var_map in
        ty_var_map := (name, id) :: !ty_var_map;
        id
  in
  let rec parse_arrow toks =
    match parse_primary toks with
    | Ok (ty, "->" :: rest) -> (
        match parse_arrow rest with
        | Ok (res_ty, rem) -> Ok (Types.TyArrow (ty, res_ty), rem)
        | Error e -> Error e)
    | Ok (ty, rest) -> Ok (ty, rest)
    | Error e -> Error e
  and parse_primary = function
    | [] -> Error { Ast.span; message = "Unexpected end of type expression" }
    | "(" :: rest -> (
        match parse_arrow rest with
        | Ok (ty, ")" :: rem) -> Ok (ty, rem)
        | Ok (_, _) ->
            Error { Ast.span; message = "Unclosed parenthesis in type expression" }
        | Error e -> Error e)
    | "int" :: rest -> Ok (Types.TyInt, rest)
    | "bool" :: rest -> Ok (Types.TyBool, rest)
    | "string" :: rest -> Ok (Types.TyString, rest)
    | "unit" :: rest -> Ok (Types.TyUnit, rest)
    | tok :: rest when String.length tok > 1 && tok.[0] = '\'' ->
        Ok (Types.TyVar (get_ty_var tok), rest)
    | tok :: rest ->
        (* Treat any other named type as TyVar or primitive *)
        Ok (Types.TyVar (get_ty_var ("'" ^ tok)), rest)
  in
  match parse_arrow tokens with
  | Ok (ty, []) -> Ok ty
  | Ok (_, extra :: _) ->
      Error
        {
          Ast.span;
          message = Printf.sprintf "Unexpected trailing token in type: '%s'" extra;
        }
  | Error err -> Error err

let parse_interface ~file (content : string) : (mod_sig, Ast.error) result =
  let lines = String.split_on_char '\n' content in
  let rec process_lines line_num acc = function
    | [] ->
        let full_span =
          match
            Ast.create_span ~file ~start_line:1 ~start_col:1
              ~end_line:(max 1 (line_num - 1))
              ~end_col:1
          with
          | Ok s -> s
          | Error _ -> Ast.dummy_span
        in
        Ok (SigBody { items = List.rev acc; span = full_span })
    | raw_line :: rest -> (
        let trimmed = String.trim raw_line in
        if
          String.length trimmed = 0
          || (String.length trimmed >= 2 && String.sub trimmed 0 2 = "//")
          || (String.length trimmed >= 2 && String.sub trimmed 0 2 = "(*")
          || trimmed.[0] = '#'
        then process_lines (line_num + 1) acc rest
        else
          let span =
            match
              Ast.create_span ~file ~start_line:line_num ~start_col:1 ~end_line:line_num
                ~end_col:(String.length raw_line + 1)
            with
            | Ok s -> s
            | Error _ -> Ast.dummy_span
          in
          let words =
            String.split_on_char ' ' trimmed |> List.filter (fun s -> String.length s > 0)
          in
          match words with
          | "val" :: name :: ":" :: type_words -> (
              let type_str = String.concat " " type_words in
              match parse_type_str ~span type_str with
              | Ok ty ->
                  let item = SigVal { name; ty; span } in
                  process_lines (line_num + 1) (item :: acc) rest
              | Error err -> Error err)
          | [ "type"; name ] ->
              let item = SigType { name; params = []; manifest = None; span } in
              process_lines (line_num + 1) (item :: acc) rest
          | "type" :: name :: "=" :: type_words -> (
              let type_str = String.concat " " type_words in
              match parse_type_str ~span type_str with
              | Ok ty ->
                  let item = SigType { name; params = []; manifest = Some ty; span } in
                  process_lines (line_num + 1) (item :: acc) rest
              | Error err -> Error err)
          | [ "open"; path_str ] -> (
              match parse_path ~span path_str with
              | Ok path ->
                  let item = SigOpen { path; span } in
                  process_lines (line_num + 1) (item :: acc) rest
              | Error err -> Error err)
          | "sig" :: _ | "end" :: _ ->
              (* Block delimiters inside interface *)
              process_lines (line_num + 1) acc rest
          | _ ->
              Error
                {
                  Ast.span;
                  message =
                    Printf.sprintf "Syntax error in interface declaration: '%s'" trimmed;
                })
  in
  process_lines 1 [] lines

let parse_implementation ~file (content : string) : (mod_expr, Ast.error) result =
  let lines = String.split_on_char '\n' content in
  let rec extract_headers line_num items_acc remaining_lines =
    match remaining_lines with
    | [] -> (List.rev items_acc, [])
    | raw_line :: rest -> (
        let trimmed = String.trim raw_line in
        if
          String.length trimmed = 0
          || (String.length trimmed >= 2 && String.sub trimmed 0 2 = "//")
          || (String.length trimmed >= 2 && String.sub trimmed 0 2 = "(*")
          || trimmed.[0] = '#'
        then extract_headers (line_num + 1) items_acc rest
        else
          let span =
            match
              Ast.create_span ~file ~start_line:line_num ~start_col:1 ~end_line:line_num
                ~end_col:(String.length raw_line + 1)
            with
            | Ok s -> s
            | Error _ -> Ast.dummy_span
          in
          let words =
            String.split_on_char ' ' trimmed |> List.filter (fun s -> String.length s > 0)
          in
          match words with
          | [ "import"; path_str ] -> (
              match parse_path ~span path_str with
              | Ok path ->
                  let it = ModImport { path; alias = None; span } in
                  extract_headers (line_num + 1) (it :: items_acc) rest
              | Error _ -> (List.rev items_acc, remaining_lines))
          | [ "import"; path_str; "as"; alias ] -> (
              match parse_path ~span path_str with
              | Ok path ->
                  let it = ModImport { path; alias = Some alias; span } in
                  extract_headers (line_num + 1) (it :: items_acc) rest
              | Error _ -> (List.rev items_acc, remaining_lines))
          | [ "open"; path_str ] -> (
              match parse_path ~span path_str with
              | Ok path ->
                  let it = ModOpen { path; span } in
                  extract_headers (line_num + 1) (it :: items_acc) rest
              | Error _ -> (List.rev items_acc, remaining_lines))
          | "type" :: name :: "=" :: type_words -> (
              let type_str = String.concat " " type_words in
              match parse_type_str ~span type_str with
              | Ok ty ->
                  let it = ModType { name; params = []; ty; span } in
                  extract_headers (line_num + 1) (it :: items_acc) rest
              | Error _ -> (List.rev items_acc, remaining_lines))
          | _ -> (List.rev items_acc, remaining_lines))
  in
  let header_items, code_lines = extract_headers 1 [] lines in
  let code_text = String.concat "\n" code_lines in
  let trimmed_code = String.trim code_text in
  let span =
    match
      Ast.create_span ~file ~start_line:1 ~start_col:1
        ~end_line:(max 1 (List.length lines))
        ~end_col:1
    with
    | Ok s -> s
    | Error _ -> Ast.dummy_span
  in
  if String.length trimmed_code = 0 then Ok (ModStruct { items = header_items; span })
  else
    match Parse_facade.parse_string ~file code_text with
    | Ok prog ->
        let code_items = of_ast_decls prog.decls in
        Ok (ModStruct { items = header_items @ code_items; span })
    | Error diag ->
        Error { Ast.span = diag.Diagnostic.span; message = diag.Diagnostic.message }

let parse_compilation_unit ~file (content : string) : (compilation_unit, Ast.error) result
    =
  let base_name =
    Filename.basename file |> Filename.remove_extension |> String.capitalize_ascii
  in
  let span =
    match Ast.create_span ~file ~start_line:1 ~start_col:1 ~end_line:1 ~end_col:1 with
    | Ok s -> s
    | Error _ -> Ast.dummy_span
  in
  if Filename.check_suffix file ".vesperi" then
    match parse_interface ~file content with
    | Ok sign -> Ok { name = base_name; file_path = file; kind = Interface sign; span }
    | Error err -> Error err
  else
    match parse_implementation ~file content with
    | Ok expr ->
        Ok { name = base_name; file_path = file; kind = Implementation expr; span }
    | Error err -> Error err
