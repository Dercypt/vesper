# Vesper Standard Library & Prelude

This document specifies the standard library foundation for Vesper, encompassing both the automatically injected language prelude and the pure functional standard modules designed for host and runtime support.

---

## 1. Automatic Prelude

Every Vesper file automatically has access to the built-in pure prelude without requiring any manual `import` or `open` statement. All prelude functions adhere strictly to **Law 3** (Source Provenance) and **Law 5** (Deterministic Evaluation).

### 1.1 Arithmetic & Numeric Functions

#### `abs x`
* **Signature**: `int -> int`
* **Definition**: `if x < 0 then -x else x`
* Returns the absolute value of integer `x`.

#### `min x y`
* **Signature**: `int -> int -> int`
* **Definition**: `if x < y then x else y`
* Returns the smaller of two integers.

#### `max x y`
* **Signature**: `int -> int -> int`
* **Definition**: `if x > y then x else y`
* Returns the greater of two integers.

#### `clamp low high x`
* **Signature**: `int -> int -> int -> int`
* **Definition**: `if x < low then low else if x > high then high else x`
* Restricts integer `x` to the inclusive interval `[low, high]`.

#### `sign x`
* **Signature**: `int -> int`
* **Definition**: `if x < 0 then -1 else if x > 0 then 1 else 0`
* Returns the sign of integer `x`: `-1` if negative, `1` if positive, `0` if zero.

#### `succ x`
* **Signature**: `int -> int`
* **Definition**: `x + 1`
* Returns the successor of integer `x`.

#### `pred x`
* **Signature**: `int -> int`
* **Definition**: `x - 1`
* Returns the predecessor of integer `x`.

#### `is_zero x`
* **Signature**: `int -> bool`
* **Definition**: `x == 0`
* Returns `true` if `x` equals zero.

#### `is_positive x`
* **Signature**: `int -> bool`
* **Definition**: `x > 0`
* Returns `true` if `x` is strictly positive.

#### `is_negative x`
* **Signature**: `int -> bool`
* **Definition**: `x < 0`
* Returns `true` if `x` is strictly negative.

---

### 1.2 Boolean Operations

#### `not b`
* **Signature**: `bool -> bool`
* **Definition**: `if b then false else true`
* Inverts a boolean truth value.

---

### 1.3 Higher-Order Combinators

#### `id x`
* **Signature**: `'a -> 'a`
* **Definition**: `x`
* The identity function. Returns its argument unchanged.

#### `const x y`
* **Signature**: `'a -> 'b -> 'a`
* **Definition**: `x`
* The constant function (K-combinator). Ignores its second argument and returns the first.

#### `flip f x y`
* **Signature**: `('a -> 'b -> 'c) -> 'b -> 'a -> 'c`
* **Definition**: `f y x`
* Takes a two-argument function and returns a function with the argument order reversed.

#### `compose f g x`
* **Signature**: `('b -> 'c) -> ('a -> 'b) -> 'a -> 'c`
* **Definition**: `f (g x)`
* Standard function composition ($f \circ g$). Applies `g` to `x`, then applies `f` to the result.

---

## 2. Host Standard Library Modules (`lib/stdlib/`)

Vesper includes a suite of pure, verified standard modules implementing foundational data structures and safe operations with total isolation against host crashes (**Law 1**).

### 2.1 `Core_list` (`lib/stdlib/core_list.mli`)
Pure functional, total operations on lists:
* `length : 'a list -> int`
* `is_empty : 'a list -> bool`
* `head : 'a list -> 'a option` (safe; returns `None` on empty list)
* `tail : 'a list -> 'a list option`
* `cons : 'a -> 'a list -> 'a list`
* `map : ('a -> 'b) -> 'a list -> 'b list`
* `filter : ('a -> bool) -> 'a list -> 'a list`
* `fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc` (tail-recursive)
* `fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc`
* `reverse : 'a list -> 'a list`
* `append : 'a list -> 'a list -> 'a list`

### 2.2 `Core_map` (`lib/stdlib/core_map.mli`)
Pure immutable balanced associative maps with string keys:
* `type 'a t`
* `empty : 'a t`
* `is_empty : 'a t -> bool`
* `add : string -> 'a -> 'a t -> 'a t`
* `find_opt : string -> 'a t -> 'a option`
* `find : string -> 'a t -> ('a, string) result`
* `remove : string -> 'a t -> 'a t`
* `mem : string -> 'a t -> bool`
* `cardinal : 'a t -> int`
* `map : ('a -> 'b) -> 'a t -> 'b t`
* `fold : (string -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc`
* `to_list : 'a t -> (string * 'a) list`

### 2.3 `Core_option` (`lib/stdlib/core_option.mli`)
Safe optional value combinators eliminating null-pointer hazards:
* `some : 'a -> 'a option`
* `none : 'a option`
* `is_some : 'a option -> bool`
* `is_none : 'a option -> bool`
* `value : default:'a -> 'a option -> 'a`
* `map : ('a -> 'b) -> 'a option -> 'b option`
* `bind : 'a option -> ('a -> 'b option) -> 'b option`
* `fold : none:'b -> some:('a -> 'b) -> 'a option -> 'b`
* `to_result : error:'e -> 'a option -> ('a, 'e) result`

### 2.4 `Core_result` (`lib/stdlib/core_result.mli`)
Monadic error handling without exceptions:
* `ok : 'a -> ('a, 'e) result`
* `error : 'e -> ('a, 'e) result`
* `is_ok : ('a, 'e) result -> bool`
* `is_error : ('a, 'e) result -> bool`
* `value : default:'a -> ('a, 'e) result -> 'a`
* `map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result`
* `map_error : ('e1 -> 'e2) -> ('a, 'e1) result -> ('a, 'e2) result`
* `bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result`
* `fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) result -> 'c`
* `to_option : ('a, 'e) result -> 'a option`

### 2.5 `Core_string` (`lib/stdlib/core_string.mli`)
Immutable string manipulation with bounds safety:
* `length : string -> int`
* `is_empty : string -> bool`
* `concat : ?sep:string -> string list -> string`
* `split : on:char -> string -> string list`
* `split_lines : string -> string list`
* `trim : string -> string`
* `slice : string -> start:int -> len:int -> (string, string) result`
* `starts_with : prefix:string -> string -> bool`
* `ends_with : suffix:string -> string -> bool`

### 2.6 `Core_math` (`lib/stdlib/core_math.mli`)
Checked integer arithmetic protecting against overflow and division-by-zero crashes:
* `abs : int -> int` (safely handles `min_int`)
* `div : int -> int -> (int, string) result` (returns `Error "Division by zero"` when denominator is 0)
* `rem : int -> int -> (int, string) result`
* `pow : int -> int -> (int, string) result` (returns `Error "Negative exponent"` when exponent is negative)
