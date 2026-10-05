let length l = List.length l
let is_empty = function [] -> true | _ -> false
let head = function [] -> None | x :: _ -> Some x
let tail = function [] -> None | _ :: xs -> Some xs
let cons x xs = x :: xs
let map f l = List.map f l
let filter p l = List.filter p l
let fold_left f init l = List.fold_left f init l
let fold_right f l init = List.fold_right f l init
let reverse l = List.rev l
let append l1 l2 = List.append l1 l2
let concat l = List.concat l

let zip l1 l2 =
  let rec loop acc = function
    | x :: xs, y :: ys -> loop ((x, y) :: acc) (xs, ys)
    | _ -> List.rev acc
  in
  loop [] (l1, l2)

let zip_with f l1 l2 =
  let rec loop acc = function
    | x :: xs, y :: ys -> loop (f x y :: acc) (xs, ys)
    | _ -> List.rev acc
  in
  loop [] (l1, l2)

let take n l =
  if n <= 0 then []
  else
    let rec loop count acc = function
      | [] -> List.rev acc
      | x :: xs -> if count <= 0 then List.rev acc else loop (count - 1) (x :: acc) xs
    in
    loop n [] l

let drop n l =
  if n <= 0 then l
  else
    let rec loop count = function
      | [] -> []
      | _ :: xs when count = 1 -> xs
      | _ :: xs -> loop (count - 1) xs
    in
    loop n l

let nth n l =
  if n < 0 then None
  else
    let rec loop count = function
      | [] -> None
      | x :: _ when count = 0 -> Some x
      | _ :: xs -> loop (count - 1) xs
    in
    loop n l

let find p l = List.find_opt p l
let exists p l = List.exists p l
let for_all p l = List.for_all p l
let partition p l = List.partition p l
let flatten = concat
let equal eq l1 l2 = List.equal eq l1 l2
