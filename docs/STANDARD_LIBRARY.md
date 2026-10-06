# Standard Library

## Prelude

Injected into every Vesper program:

* `id : 'a -> 'a`
* `const : 'a -> 'b -> 'a`
* `flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c`
* `compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c`
* `abs : int -> int`
* `min : int -> int -> int`
* `max : int -> int -> int`
* `clamp : int -> int -> int -> int`
* `sign : int -> int`
* `succ : int -> int`
* `pred : int -> int`
* `is_zero : int -> bool`
* `is_positive : int -> bool`
* `is_negative : int -> bool`
* `not : bool -> bool`

## Host Modules (`lib/stdlib/`)

Pure OCaml implementations safe against host exceptions:

### `Core_list` (`lib/stdlib/core_list.mli`)
* `length`, `is_empty`, `head`, `tail`, `cons`, `map`, `filter`, `fold_left`, `fold_right`, `reverse`, `append`

### `Core_map` (`lib/stdlib/core_map.mli`)
* Immutable string-keyed map: `empty`, `is_empty`, `add`, `find_opt`, `find`, `remove`, `mem`, `cardinal`, `map`, `fold`, `to_list`

### `Core_option` (`lib/stdlib/core_option.mli`)
* `some`, `none`, `is_some`, `is_none`, `value`, `map`, `bind`, `fold`, `to_result`

### `Core_result` (`lib/stdlib/core_result.mli`)
* `ok`, `error`, `is_ok`, `is_error`, `value`, `map`, `map_error`, `bind`, `fold`, `to_option`

### `Core_string` (`lib/stdlib/core_string.mli`)
* `length`, `is_empty`, `concat`, `split`, `split_lines`, `trim`, `slice`, `starts_with`, `ends_with`

### `Core_math` (`lib/stdlib/core_math.mli`)
* Checked arithmetic: `abs`, `add`, `sub`, `mul`, `div`, `rem`, `pow`, `min`, `max`
