type ('a, 'e) t = ('a, 'e) result

let ok x = Ok x
let error e = Error e
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Error _ -> true | Ok _ -> false
let value ~default = function Ok x -> x | Error _ -> default
let map f = function Ok x -> Ok (f x) | Error e -> Error e
let map_error f = function Ok x -> Ok x | Error e -> Error (f e)
let bind res f = match res with Ok x -> f x | Error e -> Error e
let fold ~ok:ok_fn ~error:err_fn = function Ok x -> ok_fn x | Error e -> err_fn e
let to_option = function Ok x -> Some x | Error _ -> None
let equal eq_val eq_err r1 r2 = Result.equal ~ok:eq_val ~error:eq_err r1 r2
