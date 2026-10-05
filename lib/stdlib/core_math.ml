let abs n = if n = Int.min_int then Int.max_int else if n < 0 then -n else n
let add a b = a + b
let sub a b = a - b
let mul a b = a * b

let div a b =
  if b = 0 then Error "Division by zero"
  else if a = Int.min_int && b = -1 then Ok Int.max_int
  else Ok (a / b)

let rem a b =
  if b = 0 then Error "Division by zero"
  else if a = Int.min_int && b = -1 then Ok 0
  else Ok (a mod b)

let pow base exp =
  if exp < 0 then Error "Negative exponent"
  else if exp = 0 then Ok 1
  else
    let rec loop acc b e =
      if e = 0 then acc
      else if e mod 2 = 1 then loop (acc * b) (b * b) (e / 2)
      else loop acc (b * b) (e / 2)
    in
    Ok (loop 1 base exp)

let min (a : int) (b : int) = if a < b then a else b
let max (a : int) (b : int) = if a > b then a else b

let clamp ~min:lo ~max:hi n =
  if lo > hi then lo else if n < lo then lo else if n > hi then hi else n

let sign n = if n < 0 then -1 else if n > 0 then 1 else 0
let succ n = n + 1
let pred n = n - 1
let is_zero n = n = 0
let is_positive n = n > 0
let is_negative n = n < 0
let is_even n = n mod 2 = 0
let is_odd n = n mod 2 <> 0
