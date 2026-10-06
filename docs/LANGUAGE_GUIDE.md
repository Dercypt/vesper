# Vesper Language Guide & Reference

This guide provides a comprehensive specification of the **Vesper** programming language syntax, type system, evaluation semantics, and standard conventions.

---

## 1. Overview & Core Philosophy

Vesper is a strongly typed, purely functional language designed with an emphasis on mathematical invariants:
* **Expressions over Statements**: Everything in Vesper is an expression that yields a value.
* **Deterministic Evaluation**: Program evaluation is referentially transparent and bounded by execution fuel to prevent non-terminating host lockups (Law 5).
* **Total Type Safety**: Hindley-Milner type inference ensures programs are statically type-checked with zero runtime type errors.
* **Invertible Syntax**: Canonical formatting and parsing roundtrip without information loss (Law 2).

---

## 2. Lexical Structure

### 2.1 Identifiers
Identifiers in Vesper begin with a lowercase letter or underscore, followed by alphanumeric characters or underscores:
```vesper
let count = 10;
let user_name = "Alice";
let _internal = true;
```

### 2.2 Comments
Vesper supports two comment styles:
* Single-line comments starting with `//`:
  ```vesper
  // This is a single-line comment
  ```
* Multi-line block comments enclosed in `(* ... *)`:
  ```vesper
  (* This is a multi-line
     block comment *)
  ```

### 2.3 Semicolons
Top-level declarations in Vesper must terminate with a semicolon (`;`):
```vesper
let x = 10;
let y = 20;
x + y;
```

---

## 3. Literals & Types

### 3.1 Primitive Types

| Type | Examples | Description |
| :--- | :--- | :--- |
| `int` | `0`, `42`, `-17`, `65536` | Signed machine integers. |
| `bool` | `true`, `false` | Boolean truth values. |
| `string` | `"hello"`, `"line 1\nline 2"`, `""` | Immutable UTF-8/ASCII byte sequences with escape sequences (`\n`, `\t`, `\\`, `\"`). |
| `'a -> 'b` | `fun x -> x + 1` | Pure function types mapping domain to codomain. |

### 3.2 Type Inference
Vesper employs Hindley-Milner type inference (Algorithm W). You do not need to write type annotations; types are inferred automatically and unified across call sites.

```vesper
// Inferred as: int -> int -> int
let add a b = a + b;

// Inferred as: 'a -> 'a
let identity = fun x -> x;
```

If a type error occurs, the compiler pinpoints the exact source span:
```text
Error[E2001]: type mismatch at file.vesper:1:5-1:15
  expected: int
  received: bool
```

---

## 4. Expressions & Operators

### 4.1 Operator Precedence & Associativity

The following table lists Vesper operators in order from lowest precedence to highest:

| Precedence | Operator | Associativity | Description |
| :--- | :--- | :--- | :--- |
| **1 (Lowest)** | `\|\|` | Left | Logical OR |
| **2** | `&&` | Left | Logical AND |
| **3** | `==`, `!=` | Non-associative | Structural equality and inequality |
| **4** | `<`, `<=`, `>`, `>=` | Non-associative | Numerical comparisons |
| **5** | `+`, `-` | Left | Addition, subtraction, string concatenation |
| **6** | `*`, `/`, `%` | Left | Multiplication, integer division, modulo |
| **7** | `!`, `-` (unary) | Right | Logical negation, arithmetic negation |
| **8 (Highest)**| Function call | Left | Curried function application `f x y` |

### 4.2 Arithmetic Operators
* `+` : Adds two integers (or concatenates two strings).
* `-` : Subtracts two integers, or negates an integer.
* `*` : Multiplies two integers.
* `/` : Integer division (division by zero returns `0` safely under total semantics or can be guarded).
* `%` : Modulo / remainder.

```vesper
let a = 10 + 20;    // 30
let b = 15 * 3;     // 45
let c = 100 / 4;    // 25
let d = 17 % 5;     // 2
```

### 4.3 Comparison Operators
Comparisons yield a `bool`:
```vesper
let eq = (10 == 10);     // true
let neq = (10 != 5);     // true
let lt = (5 < 10);       // true
let gte = (20 >= 20);    // true
```

### 4.4 Logical Operators
Logical operations accept boolean operands:
```vesper
let t = true && true;    // true
let f = false || false;  // false
let n = !true;           // false
```

### 4.5 String Concatenation
Strings are concatenated using the `+` operator:
```vesper
let greeting = "Hello, " + "world!";
```

---

## 5. Functions & Bindings

### 5.1 Variable Bindings
```vesper
let message = "Vesper";
let answer = 42;
```

### 5.2 Curried Functions
Functions are first-class and curried by default:
```vesper
let add a b = a + b;
let add_five = add 5;
let result = add_five 10; // 15
```

### 5.3 Lambda Expressions (`fun`)
Anonymous functions use the `fun <arg> -> <body` syntax:
```vesper
let double = fun x -> x * 2;
let multiply = fun x y -> x * y;
```

### 5.4 Recursive Functions (`let rec`)
Recursive functions must be declared explicitly with `let rec`:
```vesper
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1);

let rec power base exp =
  if exp <= 0 then 1
  else base * power base (exp - 1);
```

### 5.5 Mutual & Structural Algorithms
Vesper allows implementing complex algorithms through tail-recursive or structural recursive patterns:
```vesper
let rec gcd a b =
  if b == 0 then a
  else gcd b (a % b);

let lcm a b =
  if a == 0 || b == 0 then 0
  else (a * b) / (gcd a b);
```

---

## 6. Control Flow: Conditionals

Conditionals in Vesper are expressions. Both the `then` and `else` branches are mandatory and must evaluate to expressions of identical types:

```vesper
let max a b =
  if a > b then a
  else b;

let status =
  if score >= 90 then "Grade A"
  else if score >= 80 then "Grade B"
  else "Needs Improvement";
```

---

## 7. Standard Prelude Combinators

Vesper automatically loads an internal pure prelude for every program, providing fundamental combinators:

| Combinator | Type | Implementation | Description |
| :--- | :--- | :--- | :--- |
| `id` | `'a -> 'a` | `fun x -> x` | Identity function |
| `const` | `'a -> 'b -> 'a` | `fun x y -> x` | Constant function |
| `flip` | `('a -> 'b -> 'c) -> 'b -> 'a -> 'c` | `fun f x y -> f y x` | Swaps arguments |
| `compose` | `('b -> 'c) -> ('a -> 'b) -> 'a -> 'c` | `fun f g x -> f (g x)` | Function composition |
| `min` | `int -> int -> int` | `fun a b -> if a < b then a else b` | Minimum of two integers |
| `max` | `int -> int -> int` | `fun a b -> if a > b then a else b` | Maximum of two integers |
| `clamp` | `int -> int -> int -> int` | `fun lo hi x -> min hi (max lo x)` | Clamps value in range `[lo, hi]` |
| `sign` | `int -> int` | `fun x -> if x > 0 then 1 else ...` | Signum of an integer (-1, 0, 1) |
| `succ` | `int -> int` | `fun x -> x + 1` | Successor |
| `pred` | `int -> int` | `fun x -> x - 1` | Predecessor |
| `not` | `bool -> bool` | `fun b -> if b then false else true` | Boolean inverter |

---

## 8. Idiomatic Vesper Example

Here is a complete, standalone Vesper program demonstrating currying, recursion, string concatenation, and formatting:

```vesper
// Define higher-order combinator
let apply_twice f x = f (f x);

// Define arithmetic helpers
let double x = x * 2;
let inc x = x + 1;

// Define recursive integer square root
let rec isqrt_guess n g =
  if g * g <= n && (g + 1) * (g + 1) > n then g
  else if g == 0 then 0
  else isqrt_guess n ((g + n / g) / 2);

let isqrt n =
  if n <= 0 then 0
  else isqrt_guess n (n / 2 + 1);

// Test computations
let num = 256;
let root = isqrt num;
let doubled_root = apply_twice double root;

// Output final result
doubled_root;
```
