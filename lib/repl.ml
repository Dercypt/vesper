type t = { type_env : Type_env.t; eval_env : Env.t }

let initial () =
  { type_env = Prelude.default_type_env (); eval_env = Prelude.default_eval_env () }

let internal_error msg =
  Diagnostic.error ~code:Error_code.E0002_internal_error ~span:Ast.dummy_span msg

let render_value v = Value.to_string v

let eval_input_exn st src =
  match Parse_facade.parse_string ~file:"<repl>" src with
  | Error d -> Error [ d ]
  | Ok prog -> (
      match Typecheck.typecheck_program ~env:st.type_env prog with
      | Error ds -> Error ds
      | Ok typed -> (
          match Desugar.lower prog with
          | Error e -> Error [ Diagnostic.of_ast_error e ]
          | Ok core -> (
              if List.length core.decls <> List.length typed.decls then
                Error [ internal_error "REPL declaration count mismatch" ]
              else
                let rec go env lines = function
                  | [] -> Ok (env, List.rev lines)
                  | ((td : Typecheck.typed_decl), cd) :: rest -> (
                      match Eval.eval_decl env cd with
                      | Error e -> Error [ Diagnostic.of_ast_error e ]
                      | Ok (v, env') ->
                          let line =
                            match td.desc with
                            | Typecheck.TypedLetDecl { name; scheme; _ } ->
                                Printf.sprintf "val %s : %s = %s" name
                                  (Types.scheme_to_string (Types.normalize_scheme scheme))
                                  (render_value v)
                            | Typecheck.TypedExprDecl e ->
                                Printf.sprintf "- : %s = %s"
                                  (Types.to_string (Types.normalize_vars e.ty))
                                  (render_value v)
                          in
                          go env' (line :: lines) rest)
                in
                match go st.eval_env [] (List.combine typed.decls core.decls) with
                | Error ds -> Error ds
                | Ok (eval_env, lines) ->
                    Ok ({ type_env = typed.env; eval_env }, String.concat "\n" lines))))

let eval_input st src =
  try eval_input_exn st src
  with exn ->
    Error [ internal_error ("Unhandled host exception: " ^ Printexc.to_string exn) ]

let ends_with_semicolon s =
  let t = String.trim s in
  t <> "" && t.[String.length t - 1] = ';'

let run ~read_line ~out ~err =
  let report src ds =
    List.iter
      (fun d -> err (Diagnostic.render_terminal ~use_color:false ~source:src d ^ "\n"))
      ds
  in
  let submit st src =
    match eval_input st src with
    | Ok (st', text) ->
        if text <> "" then out (text ^ "\n");
        st'
    | Error ds ->
        report src ds;
        st
  in
  let rec loop st buf =
    out (if buf = "" then "vesper> " else "   ...> ");
    match read_line () with
    | None ->
        if String.trim buf <> "" then ignore (submit st buf);
        out "\n";
        0
    | Some line ->
        let trimmed = String.trim line in
        if buf = "" && (trimmed = ":quit" || trimmed = ":q") then 0
        else if buf = "" && trimmed = "" then loop st ""
        else if buf = "" && trimmed = ":help" then begin
          out "Enter declarations or expressions terminated by ';'. Use :quit to exit.\n";
          loop st ""
        end
        else
          let buf' = if buf = "" then line else buf ^ "\n" ^ line in
          if ends_with_semicolon buf' then loop (submit st buf') "" else loop st buf'
  in
  loop (initial ()) ""
