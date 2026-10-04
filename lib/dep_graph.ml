module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type import_info = { module_name : string; span : Ast.span }
type edge = { target : string; span : Ast.span }

type node = {
  name : string;
  span : Ast.span;
  file_path : string option;
  dependencies : edge list;
  dependents : string list;
}

type t = node StringMap.t

let empty : t = StringMap.empty
let is_empty (g : t) = StringMap.is_empty g

let add_node ~name ~span ?file_path (g : t) : t =
  match StringMap.find_opt name g with
  | Some existing ->
      let updated =
        {
          existing with
          span = (if Ast.span_is_valid span then span else existing.span);
          file_path =
            (match file_path with Some _ -> file_path | None -> existing.file_path);
        }
      in
      StringMap.add name updated g
  | None ->
      let n = { name; span; file_path; dependencies = []; dependents = [] } in
      StringMap.add name n g

let add_dependency ~from_node ~to_node ~span (g : t) : t =
  let g1 = if StringMap.mem from_node g then g else add_node ~name:from_node ~span g in
  let g2 = if StringMap.mem to_node g1 then g1 else add_node ~name:to_node ~span g1 in
  let from_entry = StringMap.find from_node g2 in
  let to_entry = StringMap.find to_node g2 in
  let has_edge =
    List.exists (fun (e : edge) -> e.target = to_node) from_entry.dependencies
  in
  if has_edge then g2
  else if from_node = to_node then
    let entry' =
      {
        from_entry with
        dependencies = { target = to_node; span } :: from_entry.dependencies;
        dependents = from_node :: from_entry.dependents;
      }
    in
    StringMap.add from_node entry' g2
  else
    let from_entry' =
      {
        from_entry with
        dependencies = { target = to_node; span } :: from_entry.dependencies;
      }
    in
    let to_entry' = { to_entry with dependents = from_node :: to_entry.dependents } in
    StringMap.add to_node to_entry' (StringMap.add from_node from_entry' g2)

let mem name (g : t) = StringMap.mem name g

let nodes (g : t) =
  StringMap.fold (fun k _ acc -> k :: acc) g [] |> List.sort String.compare

let node_span name (g : t) =
  match StringMap.find_opt name g with Some n -> Some n.span | None -> None

let node_name name (g : t) =
  match StringMap.find_opt name g with Some n -> Some n.name | None -> None

let node_file name (g : t) =
  match StringMap.find_opt name g with Some n -> n.file_path | None -> None

let dependency_span ~from_node ~to_node (g : t) =
  match StringMap.find_opt from_node g with
  | Some n -> (
      match List.find_opt (fun (e : edge) -> e.target = to_node) n.dependencies with
      | Some e -> Some e.span
      | None -> None)
  | None -> None

let dependencies_of name (g : t) =
  match StringMap.find_opt name g with
  | Some n ->
      List.map (fun (e : edge) -> e.target) n.dependencies
      |> List.sort_uniq String.compare
  | None -> []

let dependents_of name (g : t) =
  match StringMap.find_opt name g with
  | Some n -> List.sort_uniq String.compare n.dependents
  | None -> []

(** {1 Fast Import Scanner (Mini-Goal 7.2, Step 7.2.1)} *)

let scan_imports_from_string ~file (content : string) :
    (import_info list, Ast.error) result =
  let lines = String.split_on_char '\n' content in
  let rec scan line_num in_block_comment acc = function
    | [] -> Ok (List.rev acc)
    | raw_line :: rest -> (
        let trimmed = String.trim raw_line in
        let len = String.length trimmed in
        if len = 0 then scan (line_num + 1) in_block_comment acc rest
        else if in_block_comment then
          match String.split_on_char '*' trimmed with
          | _ when String.length trimmed >= 2 && String.sub trimmed (len - 2) 2 = "*)" ->
              scan (line_num + 1) false acc rest
          | _ -> scan (line_num + 1) true acc rest
        else if len >= 2 && String.sub trimmed 0 2 = "(*" then
          if len >= 4 && String.sub trimmed (len - 2) 2 = "*)" then
            scan (line_num + 1) false acc rest
          else scan (line_num + 1) true acc rest
        else if (len >= 2 && String.sub trimmed 0 2 = "//") || trimmed.[0] = '#' then
          scan (line_num + 1) false acc rest
        else
          let tokens =
            String.split_on_char ' ' trimmed |> List.filter (fun s -> String.length s > 0)
          in
          let extract_module_name str =
            let clean =
              if String.contains str ';' then String.sub str 0 (String.index str ';')
              else str
            in
            match String.split_on_char '.' clean with
            | root :: _ when String.length root > 0 -> root
            | _ -> clean
          in
          let create_import_info mod_name =
            let start_col =
              match String.index_opt raw_line mod_name.[0] with
              | Some idx -> idx + 1
              | None -> 1
            in
            let end_col = start_col + String.length mod_name in
            let span =
              match
                Ast.create_span ~file ~start_line:line_num ~start_col ~end_line:line_num
                  ~end_col
              with
              | Ok s -> s
              | Error _ -> Ast.dummy_span
            in
            { module_name = String.capitalize_ascii mod_name; span }
          in
          match tokens with
          | ("import" | "open") :: mod_str :: _ ->
              let mod_name = extract_module_name mod_str in
              let info = create_import_info mod_name in
              scan (line_num + 1) false (info :: acc) rest
          | "from" :: mod_str :: "import" :: _ ->
              let mod_name = extract_module_name mod_str in
              let info = create_import_info mod_name in
              scan (line_num + 1) false (info :: acc) rest
          | _ -> scan (line_num + 1) false acc rest)
  in
  scan 1 false [] lines

let scan_imports_from_file (file_path : string) : (import_info list, Ast.error) result =
  let dummy_span =
    match
      Ast.create_span ~file:file_path ~start_line:1 ~start_col:1 ~end_line:1 ~end_col:1
    with
    | Ok s -> s
    | Error _ -> Ast.dummy_span
  in
  try
    let ic = open_in file_path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    scan_imports_from_string ~file:file_path content
  with
  | Sys_error msg ->
      Error
        {
          Ast.span = dummy_span;
          message = Printf.sprintf "I/O error reading file '%s': %s" file_path msg;
        }
  | exn ->
      Error
        {
          Ast.span = dummy_span;
          message =
            Printf.sprintf "Unexpected failure reading file '%s': %s" file_path
              (Printexc.to_string exn);
        }

(** {1 Graph Construction} *)

let of_units (units : Mod_ast.compilation_unit list) : (t, Ast.error) result =
  let g0 =
    List.fold_left
      (fun acc (u : Mod_ast.compilation_unit) ->
        add_node ~name:u.name ~span:u.span ~file_path:u.file_path acc)
      empty units
  in
  let rec process_items from_name span g = function
    | [] -> g
    | (it : Mod_ast.mod_item) :: rest -> (
        match it with
        | ModImport { path; _ } | ModOpen { path; _ } ->
            let root_mod =
              match path.segments with
              | [] -> ""
              | (s : Mod_ast.path_segment) :: _ -> String.capitalize_ascii s.name
            in
            if root_mod = "" then process_items from_name span g rest
            else
              let g' =
                add_dependency ~from_node:from_name ~to_node:root_mod ~span:path.span g
              in
              process_items from_name span g' rest
        | ModModule { expr; _ } ->
            let g' = process_expr from_name span g expr in
            process_items from_name span g' rest
        | ModVal _ | ModType _ -> process_items from_name span g rest)
  and process_expr from_name span g = function
    | Mod_ast.ModStruct { items; _ } -> process_items from_name span g items
    | Mod_ast.ModAscribed { expr; _ } -> process_expr from_name span g expr
    | Mod_ast.ModVar _ -> g
  in
  let rec process_sig_items from_name g = function
    | [] -> g
    | (it : Mod_ast.sig_item) :: rest -> (
        match it with
        | SigOpen { path; _ } ->
            let root_mod =
              match path.segments with
              | [] -> ""
              | (s : Mod_ast.path_segment) :: _ -> String.capitalize_ascii s.name
            in
            if root_mod = "" then process_sig_items from_name g rest
            else
              let g' =
                add_dependency ~from_node:from_name ~to_node:root_mod ~span:path.span g
              in
              process_sig_items from_name g' rest
        | _ -> process_sig_items from_name g rest)
  in
  let g_final =
    List.fold_left
      (fun g (u : Mod_ast.compilation_unit) ->
        match u.kind with
        | Implementation expr -> process_expr u.name u.span g expr
        | Interface (SigBody { items; _ }) -> process_sig_items u.name g items
        | Interface (SigIdent _) -> g)
      g0 units
  in
  Ok g_final

let build_from_files (file_paths : string list) : (t, Ast.error) result =
  let rec process_files acc_graph = function
    | [] -> Ok acc_graph
    | file :: rest -> (
        let mod_name =
          Filename.basename file |> Filename.remove_extension |> String.capitalize_ascii
        in
        match scan_imports_from_file file with
        | Ok imports ->
            let file_span =
              match
                Ast.create_span ~file ~start_line:1 ~start_col:1 ~end_line:1 ~end_col:1
              with
              | Ok s -> s
              | Error _ -> Ast.dummy_span
            in
            let g_with_node =
              add_node ~name:mod_name ~span:file_span ~file_path:file acc_graph
            in
            let g_with_edges =
              List.fold_left
                (fun g (imp : import_info) ->
                  add_dependency ~from_node:mod_name ~to_node:imp.module_name
                    ~span:imp.span g)
                g_with_node imports
            in
            process_files g_with_edges rest
        | Error err -> Error err)
  in
  process_files empty file_paths

(** {1 Tarjan SCC & Cycle Detection (Mini-Goal 7.2, Steps 7.2.3 & 7.2.4)} *)

let find_sccs (g : t) : string list list =
  let index = ref 0 in
  let stack = ref [] in
  let on_stack = ref StringSet.empty in
  let indices = ref StringMap.empty in
  let lowlink = ref StringMap.empty in
  let sccs = ref [] in

  let rec strongconnect (v : string) =
    indices := StringMap.add v !index !indices;
    lowlink := StringMap.add v !index !lowlink;
    incr index;
    stack := v :: !stack;
    on_stack := StringSet.add v !on_stack;

    let neighbors = dependencies_of v g in
    List.iter
      (fun w ->
        if not (StringMap.mem w !indices) then begin
          strongconnect w;
          let v_low = StringMap.find v !lowlink in
          let w_low = StringMap.find w !lowlink in
          lowlink := StringMap.add v (min v_low w_low) !lowlink
        end
        else if StringSet.mem w !on_stack then
          let v_low = StringMap.find v !lowlink in
          let w_idx = StringMap.find w !indices in
          lowlink := StringMap.add v (min v_low w_idx) !lowlink)
      neighbors;

    if StringMap.find v !lowlink = StringMap.find v !indices then begin
      let rec pop_scc acc =
        match !stack with
        | [] -> acc
        | w :: rest ->
            stack := rest;
            on_stack := StringSet.remove w !on_stack;
            if w = v then w :: acc else pop_scc (w :: acc)
      in
      let scc = pop_scc [] in
      sccs := scc :: !sccs
    end
  in

  let all_nodes = nodes g in
  List.iter (fun v -> if not (StringMap.mem v !indices) then strongconnect v) all_nodes;

  List.rev !sccs

let find_cycles (g : t) : string list list =
  let sccs = find_sccs g in
  let cycles = ref [] in
  List.iter
    (fun scc ->
      match scc with
      | [] -> ()
      | [ single ] ->
          (* Check self-loop *)
          let deps = dependencies_of single g in
          if List.mem single deps then cycles := [ single; single ] :: !cycles
      | multiple -> (
          (* Reconstruct a cycle path among nodes in the SCC *)
          let scc_set =
            List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty multiple
          in
          let start_node = List.hd multiple in
          let rec dfs_find_path current visited path =
            let neighbors =
              dependencies_of current g |> List.filter (fun n -> StringSet.mem n scc_set)
            in
            if List.mem start_node neighbors && List.length path >= 1 then
              Some (List.rev (start_node :: current :: path))
            else
              let rec try_neighbors = function
                | [] -> None
                | n :: rest -> (
                    if StringSet.mem n visited then try_neighbors rest
                    else
                      match
                        dfs_find_path n (StringSet.add n visited) (current :: path)
                      with
                      | Some res -> Some res
                      | None -> try_neighbors rest)
              in
              try_neighbors neighbors
          in
          match dfs_find_path start_node (StringSet.singleton start_node) [] with
          | Some cycle_path -> cycles := cycle_path :: !cycles
          | None ->
              (* Fallback: return sorted nodes as cycle loop *)
              let loop = multiple @ [ List.hd multiple ] in
              cycles := loop :: !cycles))
    sccs;
  List.rev !cycles

let detect_cycles (g : t) : (unit, Ast.error) result =
  let cycles = find_cycles g in
  match cycles with
  | [] -> Ok ()
  | cycle :: _ ->
      let cycle_str = String.concat " -> " cycle in
      let start_node = List.hd cycle in
      let second_node = match cycle with _ :: s :: _ -> s | _ -> start_node in
      let span =
        match dependency_span ~from_node:start_node ~to_node:second_node g with
        | Some s -> s
        | None -> (
            match node_span start_node g with Some s -> s | None -> Ast.dummy_span)
      in
      Error
        { Ast.span; message = Printf.sprintf "Cyclic dependency detected: %s" cycle_str }

(** {1 Topological Sort & Build Ordering (Mini-Goal 7.2, Step 7.2.5)} *)

let topological_sort (g : t) : (string list, Ast.error) result =
  match detect_cycles g with
  | Error err -> Error err
  | Ok () -> (
      (* Kahn's algorithm: in-degree = number of unsatisfied dependencies *)
      let all_nodes = nodes g in
      let in_degree = ref StringMap.empty in
      List.iter
        (fun u ->
          let deps = dependencies_of u g in
          in_degree := StringMap.add u (List.length deps) !in_degree)
        all_nodes;

      let queue =
        ref
          (List.filter (fun u -> StringMap.find u !in_degree = 0) all_nodes
          |> List.sort String.compare)
      in

      let sorted = ref [] in

      while !queue <> [] do
        let u = List.hd !queue in
        queue := List.tl !queue;
        sorted := u :: !sorted;

        let dependents = dependents_of u g in
        List.iter
          (fun v ->
            let cur = StringMap.find v !in_degree in
            let next = cur - 1 in
            in_degree := StringMap.add v next !in_degree;
            if next = 0 then queue := List.sort String.compare (v :: !queue))
          dependents
      done;

      if List.length !sorted = List.length all_nodes then Ok (List.rev !sorted)
      else
        (* Cyclic fallback *)
        detect_cycles g |> function
        | Error err -> Error err
        | Ok () -> Ok (List.rev !sorted))
