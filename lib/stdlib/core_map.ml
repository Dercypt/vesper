module M = Map.Make (String)

type 'a t = 'a M.t

let empty = M.empty
let is_empty = M.is_empty
let add k v m = M.add k v m
let find_opt k m = M.find_opt k m

let find k m =
  match M.find_opt k m with
  | Some v -> Ok v
  | None -> Error (Printf.sprintf "Key not found: %s" k)

let remove k m = M.remove k m
let mem k m = M.mem k m
let cardinal m = M.cardinal m
let map f m = M.map f m
let mapi f m = M.mapi f m
let fold f m init = M.fold f m init
let iter f m = M.iter f m
let to_list m = M.bindings m
let of_list l = List.fold_left (fun acc (k, v) -> M.add k v acc) M.empty l
let keys m = List.map fst (M.bindings m)
let values m = List.map snd (M.bindings m)
let equal eq m1 m2 = M.equal eq m1 m2
