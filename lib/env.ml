type t = Value.env

let empty : t = []
let extend name value (env : t) : t = (name, value) :: env
let shadow = extend
let lookup name (env : t) : Value.t option = List.assoc_opt name env

let extend_rec name param body (outer : t) : t =
  let fv =
    List.filter
      (fun v -> not (String.equal v param || String.equal v name))
      (Core_ir.free_vars body)
  in
  let filtered_outer =
    List.filter_map
      (fun v -> match lookup v outer with Some val_ -> Some (v, val_) | None -> None)
      fv
  in
  let rec closure_env =
    (name, Value.VClosure { param; body; env = closure_env }) :: filtered_outer
  in
  let closure = Value.VClosure { param; body; env = closure_env } in
  (name, closure) :: outer

let mem name (env : t) : bool = List.mem_assoc name env
let to_list (env : t) : (string * Value.t) list = env
let of_list (l : (string * Value.t) list) : t = l

let pp fmt (env : t) : unit =
  let pp_entry fmt (name, v) = Format.fprintf fmt "%s -> %a" name Value.pp v in
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_entry)
    env

let to_string (env : t) : string = Format.asprintf "%a" pp env
