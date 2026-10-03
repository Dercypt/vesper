type t = Value.env

let empty : t = []
let extend name value (env : t) : t = (name, value) :: env
let shadow = extend
let lookup name (env : t) : Value.t option = List.assoc_opt name env

let extend_rec name param body (outer : t) : t =
  let rec env = (name, Value.VClosure { param; body; env }) :: outer in
  env

let mem name (env : t) : bool = List.mem_assoc name env
let to_list (env : t) : (string * Value.t) list = env
let of_list (l : (string * Value.t) list) : t = l

let pp fmt (env : t) : unit =
  let pp_entry fmt (name, v) = Format.fprintf fmt "%s -> %a" name Value.pp v in
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_entry)
    env

let to_string (env : t) : string = Format.asprintf "%a" pp env
