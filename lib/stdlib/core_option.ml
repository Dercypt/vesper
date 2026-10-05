type 'a t = 'a option

let some x = Some x
let none = None
let is_some = function Some _ -> true | None -> false
let is_none = function None -> true | Some _ -> false
let value ~default = function Some x -> x | None -> default
let map f = function Some x -> Some (f x) | None -> None
let bind opt f = match opt with Some x -> f x | None -> None
let fold ~none:n_val ~some:s_fn = function Some x -> s_fn x | None -> n_val
let to_result ~error = function Some x -> Ok x | None -> Error error
let equal eq o1 o2 = Option.equal eq o1 o2
