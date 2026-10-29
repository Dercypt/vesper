# Language Guide

## Syntax

Top-level declarations end with `;`. Expressions are evaluated and returned.

```vesper
// Single-line comment
/* C-style multi-line comment */
(* OCaml-style multi-line
   comment *)

let x = 42;
let double a = a * 2;
let rec fact n = if n <= 1 then 1 else n * fact (n - 1);
let add = fun a b -> a + b;

fact 5;
```

## Types

* `int`: 64-bit signed integers (`42`, `-10`). Negative constants are represented in the AST and surface syntax via unary negation (`-`).
* `bool`: `true`, `false`.
* `string`: `"text"` (escapes: `\n`, `\t`, `\\`, `\"`). Concatenation via `+`.
* `'a -> 'b`: Function types. Hindley-Milner type inference (Algorithm W).
* **Overloaded Operators & Deferred Defaulting**: `+` and comparisons (`<`, `<=`, `>`, `>=`) operate on both `int` and `string`. When operands are unresolved type variables, defaulting to `int` is deferred until `generalize` at the enclosing `let` or expression boundary, making type inference order-independent for compound expressions (e.g., `let shout s = (s + s) + "!";`).

## Operators

| Operator | Associativity | Description |
| :--- | :--- | :--- |
| `\|\|` | Left | Logical OR |
| `&&` | Left | Logical AND |
| `==`, `!=` | Non-assoc | Structural equality (primitives, ADTs, records, and closures by structural chunk & captured free variables) |
| `<`, `<=`, `>`, `>=` | Non-assoc | Comparisons (integers and strings) |
| `+`, `-` | Left | Addition, string concatenation, subtraction |
| `*`, `/`, `%` | Left | Multiplication, division, modulo |
| `!`, `-` (unary) | Right | Logical negation, arithmetic negation |

## Control Flow

Conditionals are expressions and require both branches:

```vesper
let abs_val x =
  if x < 0 then -x
  else x;
```

## Prelude

Loaded automatically into every file:

| Function | Type | Behavior |
| :--- | :--- | :--- |
| `id` | `'a -> 'a` | Identity |
| `const` | `'a -> 'b -> 'a` | Constant |
| `flip` | `('a -> 'b -> 'c) -> 'b -> 'a -> 'c` | Argument swap |
| `compose` | `('b -> 'c) -> ('a -> 'b) -> 'a -> 'c` | Composition |
| `abs` | `int -> int` | Absolute value |
| `min`, `max` | `int -> int -> int` | Minimum / maximum |
| `clamp` | `int -> int -> int -> int` | Range clamp |
| `sign` | `int -> int` | Signum (`-1`, `0`, `1`) |
| `succ`, `pred` | `int -> int` | `+ 1` / `- 1` |
| `is_zero`, `is_positive`, `is_negative` | `int -> bool` | Predicates |
| `not` | `bool -> bool` | Boolean inversion |
