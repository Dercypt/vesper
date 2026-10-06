# Vesper Scientific Calculator Showcase

To prove that Vesper is a practical, functioning programming language, the repository includes a complete **Scientific Calculator with an ASCII Terminal UI** implemented in 100% pure Vesper (`calculator.vesper`) alongside an interactive driver script (`run_calculator.sh`).

---

## 1. What This Showcase Demonstrates

The Scientific Calculator exercises every layer of the Vesper compiler and bytecode runtime:
* **Hindley-Milner Type Inference**: Automatic type unification for integer arithmetic, curried functions, and string operations.
* **Complex Recursion**:
  * Exponentiation ($b^e$) via recursive descent.
  * Factorials ($n!$) with edge-case handling.
  * Integer square root ($\lfloor\sqrt{n}\rfloor$) using the Newton-Raphson approximation method.
  * Euclidean Greatest Common Divisor ($\gcd(a, b)$) and Least Common Multiple ($\text{lcm}(a, b)$).
  * Fibonacci sequences ($F_n$).
* **Pure String Formatting from Scratch**:
  * Because Vesper enforces strict type boundaries, converting numbers to strings is implemented directly in Vesper using integer division and modulo (`digit_to_str`, `int_to_str_pos`, `int_to_str`).
* **ASCII Terminal Dashboard Generation**:
  * Constructs a multi-line formatted retro LCD display card with keypad matrices and historical calculation tape using string concatenation.
* **Bytecode Virtual Machine Execution**:
  * Compiles to bytecode and executes on the Vesper stack VM with full fuel safety and zero host crashes.

---

## 2. Code Architecture Walkthrough

The program is organized into 4 logical sections in [`calculator.vesper`](../calculator.vesper):

### 2.1 Arithmetic & Scientific Math Engine
```vesper
let add a b = a + b;
let sub a b = a - b;
let mul a b = a * b;
let div a b = if b == 0 then 0 else a / b;
let modulo a b = if b == 0 then 0 else a % b;

let rec pow b e =
  if e <= 0 then 1
  else b * pow b (e - 1);

let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1);

let rec isqrt_guess n g =
  if g * g <= n && (g + 1) * (g + 1) > n then g
  else if g == 0 then 0
  else isqrt_guess n ((g + n / g) / 2);

let isqrt n =
  if n <= 0 then 0
  else isqrt_guess n (n / 2 + 1);

let rec gcd a b =
  if b == 0 then a
  else gcd b (a % b);

let lcm a b =
  if a == 0 || b == 0 then 0
  else (a * b) / (gcd a b);

let rec fib n =
  if n <= 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2);
```

### 2.2 Custom String Serialization
Converting an integer to its decimal string representation in pure Vesper:
```vesper
let digit_to_str d =
  if d == 0 then "0"
  else if d == 1 then "1"
  else if d == 2 then "2"
  else if d == 3 then "3"
  else if d == 4 then "4"
  else if d == 5 then "5"
  else if d == 6 then "6"
  else if d == 7 then "7"
  else if d == 8 then "8"
  else if d == 9 then "9"
  else "?";

let rec int_to_str_pos n =
  if n < 10 then digit_to_str n
  else "" + (int_to_str_pos (n / 10)) + (digit_to_str (n % 10));

let int_to_str n =
  if n < 0 then "-" + (int_to_str_pos (-n))
  else int_to_str_pos n;
```

### 2.3 Demonstration Calculations
Calculates a series of scientific expressions:
```vesper
let c1_expr = "125 + 75";
let c1_ans = add 125 75;

let c2_expr = "48 * 25";
let c2_ans = mul 48 25;

let c3_expr = "pow 2 10";
let c3_ans = pow 2 10;

let c4_expr = "fact 6";
let c4_ans = fact 6;

let c5_expr = "isqrt 65536";
let c5_ans = isqrt 65536;

let c6_expr = "gcd 1071 462";
let c6_ans = gcd 1071 462;

let c7_expr = "lcm 24 36";
let c7_ans = lcm 24 36;

let c8_expr = "fib 10";
let c8_ans = fib 10;

let active_expr = "(pow 2 8 + fact 6) / 10";
let active_ans = div (add (pow 2 8) (fact 6)) 10;
```

### 2.4 Terminal UI Layout Rendering
Generates a complete ASCII display layout and returns it as the program's final evaluated expression:
```vesper
let nl = "\n";
let border = "+---------------------------------------------------+" + nl;

let calc_ui =
  border +
  "|         VESPER SCIENTIFIC CALCULATOR  v1.0        |" + nl +
  border +
  "|  [ LCD DISPLAY SCREEN ]              MODE: INTEGER |" + nl +
  "|  +---------------------------------------------+  |" + nl +
  "|  | EXPRESSION : " + active_expr + "     |  |" + nl +
  "|  | RESULT     : = " + (int_to_str active_ans) + "                          |  |" + nl +
  "|  +---------------------------------------------+  |" + nl +
  border +
  "|  [ KEYPAD MATRIX ]                                |" + nl +
  "|   [ 7 ]   [ 8 ]   [ 9 ]   |   [ / ]   [ C ]   [OFF] |" + nl +
  "|   [ 4 ]   [ 5 ]   [ 6 ]   |   [ * ]   [ ( ]   [POW] |" + nl +
  "|   [ 1 ]   [ 2 ]   [ 3 ]   |   [ - ]   [ ) ]   [SQRT]|" + nl +
  "|   [ 0 ]   [ANS]   [ = ]   |   [ + ]   [MOD]   [FACT]|" + nl +
  border +
  "|  [ CALCULATION TAPE / HISTORY ]                   |" + nl +
  "|   #1 | " + c1_expr + "             = " + (int_to_str c1_ans) + "                 |" + nl +
  "|   #2 | " + c2_expr + "             = " + (int_to_str c2_ans) + "                |" + nl +
  "|   #3 | " + c3_expr + "            = " + (int_to_str c3_ans) + "                |" + nl +
  "|   #4 | " + c4_expr + "              = " + (int_to_str c4_ans) + "                 |" + nl +
  "|   #5 | " + c5_expr + "         = " + (int_to_str c5_ans) + "                 |" + nl +
  "|   #6 | " + c6_expr + "        = " + (int_to_str c6_ans) + "                  |" + nl +
  "|   #7 | " + c7_expr + "           = " + (int_to_str c7_ans) + "                  |" + nl +
  "|   #8 | " + c8_expr + "              = " + (int_to_str c8_ans) + "                  |" + nl +
  border +
  "|  STATUS: EVALUATED SUCCESSFULLY   | FUEL: NOMINAL |" + nl +
  border;

calc_ui;
```

---

## 3. How to Run

### 3.1 One-Click Terminal Launcher
```bash
./run_calculator.sh
```
This automatically compiles the Vesper compiler (if not already built) and renders the ASCII dashboard:

```text
+---------------------------------------------------+
|         VESPER SCIENTIFIC CALCULATOR  v1.0        |
+---------------------------------------------------+
|  [ LCD DISPLAY SCREEN ]              MODE: INTEGER |
|  +---------------------------------------------+  |
|  | EXPRESSION : (pow 2 8 + fact 6) / 10     |  |
|  | RESULT     : = 97                          |  |
|  +---------------------------------------------+  |
+---------------------------------------------------+
|  [ KEYPAD MATRIX ]                                |
|   [ 7 ]   [ 8 ]   [ 9 ]   |   [ / ]   [ C ]   [OFF] |
|   [ 4 ]   [ 5 ]   [ 6 ]   |   [ * ]   [ ( ]   [POW] |
|   [ 1 ]   [ 2 ]   [ 3 ]   |   [ - ]   [ ) ]   [SQRT]|
|   [ 0 ]   [ANS]   [ = ]   |   [ + ]   [MOD]   [FACT]|
+---------------------------------------------------+
|  [ CALCULATION TAPE / HISTORY ]                   |
|   #1 | 125 + 75             = 200                 |
|   #2 | 48 * 25             = 1200                |
|   #3 | pow 2 10            = 1024                |
|   #4 | fact 6              = 720                 |
|   #5 | isqrt 65536         = 256                 |
|   #6 | gcd 1071 462        = 21                  |
|   #7 | lcm 24 36           = 72                  |
|   #8 | fib 10              = 55                  |
+---------------------------------------------------+
|  STATUS: EVALUATED SUCCESSFULLY   | FUEL: NOMINAL |
+---------------------------------------------------+
```

### 3.2 Interactive Calculator Mode
```bash
./run_calculator.sh --interactive
```
Launches an interactive prompt where you can evaluate any arithmetic or scientific expression on the fly, with LCD display card output:

```text
vesper-calc > fact 5

+---------------------------------------------------+
|  [ LCD DISPLAY ]                     STATUS: OK   |
|  +---------------------------------------------+  |
|  |  EXPR: fact 5                               |  |
|  |  ANS : = 120                                |  |
|  +---------------------------------------------+  |
+---------------------------------------------------+

vesper-calc > gcd 48 18

+---------------------------------------------------+
|  [ LCD DISPLAY ]                     STATUS: OK   |
|  +---------------------------------------------+  |
|  |  EXPR: gcd 48 18                            |  |
|  |  ANS : = 6                                  |  |
|  +---------------------------------------------+  |
+---------------------------------------------------+
```

### 3.3 Direct Toolchain Commands
```bash
# Validate syntax and types:
vesper check calculator.vesper

# Compile and run via Bytecode VM:
vesper run calculator.vesper

# Inspect compiled instructions:
vesper disasm calculator.vesper
```
