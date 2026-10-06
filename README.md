# Vesper

A pure functional programming language engineered from scratch in OCaml, governed by mathematical domain invariants and verified across 10 completed development phases.

[![Verification Harness](https://img.shields.io/badge/harness-passing-brightgreen)](verify.sh)
[![Domain Laws](https://img.shields.io/badge/laws-1%20to%205%20verified-blue)](LAWS.md)
[![Phases](https://img.shields.io/badge/phases-10%2F10%20complete-success)](roadmap.md)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 🌟 Interactive Showcase: Scientific Calculator in Pure Vesper

To prove that Vesper is a practical, functioning programming language, the repository includes a complete **Scientific Calculator with an ASCII Terminal UI** implemented in 100% pure Vesper ([`calculator.vesper`](calculator.vesper)) alongside an interactive terminal launcher ([`run_calculator.sh`](run_calculator.sh)).

### Run the Showcase:
```bash
# Display the formatted retro LCD dashboard and calculation tape:
./run_calculator.sh

# Or launch an interactive live calculation session:
./run_calculator.sh --interactive
```

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

Read the full walkthrough in [**`docs/CALCULATOR_SHOWCASE.md`**](docs/CALCULATOR_SHOWCASE.md).

---

## 📚 Documentation Suite

Explore the comprehensive Vesper documentation in [`docs/`](docs/):

* [**Documentation Portal (`docs/README.md`)**](docs/README.md): Central index of all documentation guides and references.
* [**Language Guide (`docs/LANGUAGE_GUIDE.md`)**](docs/LANGUAGE_GUIDE.md): Grammar, syntax, expressions, recursion, currying, Hindley-Milner type system, and prelude combinators.
* [**Compiler & Runtime Architecture (`docs/ARCHITECTURE.md`)**](docs/ARCHITECTURE.md): Deep dive into the 10 compiler phases: Lexer, Menhir parser, Core IR, Typechecker, ADTs, Modules, Stack Bytecode VM, and Sandboxed Effects.
* [**CLI & Tooling Reference (`docs/CLI_REFERENCE.md`)**](docs/CLI_REFERENCE.md): Complete manual for `vesper run`, `check`, `fmt`, `repl`, `disasm`, `lsp`, exit codes, and IDE integration.
* [**Standard Library & Prelude (`docs/STANDARD_LIBRARY.md`)**](docs/STANDARD_LIBRARY.md): Detailed API reference for the automatic prelude and host modules (`List`, `Map`, `Option`, `Result`, `String`, `Math`).
* [**Scientific Calculator Showcase (`docs/CALCULATOR_SHOWCASE.md`)**](docs/CALCULATOR_SHOWCASE.md): Step-by-step breakdown of the scientific calculator algorithms and terminal UI rendering.
* [**Self-Hosting Blueprint (`docs/SELF_HOSTING.md`)**](docs/SELF_HOSTING.md): Roadmap for bootstrapping Vesper within Vesper (Core Vesper and bootstrap stages 0–4).

---

## 🏛️ Architecture & Governance Harness

Vesper is governed by five non-negotiable domain invariants:

* [**`LAWS.md`**](LAWS.md): The 5 sovereign domain laws:
  1. **Total Diagnosability & Host Isolation**: User code never crashes the host runtime.
  2. **Invertible Concrete Syntax**: $\text{parse} \circ \text{print} \circ \text{parse} = \text{parse}$.
  3. **Strict Source Provenance**: AST and Core IR preserve 100% of source spans.
  4. **Idempotent Normalization**: Desugaring is strictly idempotent.
  5. **Deterministic Evaluation & Sound Termination**: Pure execution with fuel bounds.
* [**`PRINCIPLES.md`**](PRINCIPLES.md): Architectural decisions (`ocamllex` vs `sedlex`, warning policies), code defaults (`.mli` first, immutability), and escalation triggers.
* [**`HARNESS.md`**](HARNESS.md): Architecture of the verification pipeline.
* [**`AGENTS.md`**](AGENTS.md): Operational protocols for contributors and agents.
* [**`roadmap.md`**](roadmap.md): The 10-phase engineering roadmap, now 100% complete.

---

## 🚀 Quickstart

### 1. Toolchain Setup (macOS / Linux)

```bash
brew install opam
opam init -y --bare
opam switch create vesper 5.2.0
eval $(opam env)
opam install -y dune menhir ocamlformat alcotest
```

### 2. Verify Repository

Run the automated 4-layer governance verification harness:

```bash
./verify.sh
```

---

## 🛠️ CLI Usage

```bash
vesper run <file>             # typecheck, compile to bytecode, execute; prints the final value
vesper check <file>           # parse + typecheck only
vesper fmt <file>             # pretty-print to stdout
vesper fmt --write <file>     # rewrite in place
vesper disasm <file>          # bytecode with source-span mappings
vesper repl                   # interactive session (:quit to exit)
vesper lsp                    # Language Server Protocol over stdin/stdout
vesper --help | --version
```

### REPL Example:
```text
$ vesper repl
vesper> let x = 40 + 2;
val x : int = 42
vesper> x + 1;
- : int = 43
vesper> :quit
```

---

## 💻 Language Quick Look

```vesper
// Declarations end with ';'
let x = 40 + 2;

// Curried functions
let double a = a * 2;

// Recursive functions
let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1);

// Anonymous lambda expressions
let add = fun a b -> a + b;

// Top-level expression evaluated and returned
double x;
```

---

## 📂 Repository Structure

* `bin/`: CLI driver executable entry point (`main.ml`).
* `lib/`: Core compiler, typechecker, and virtual machine:
  * `lexer.mll`, `parser.mly`, `tokens.mli`: Lexing and parsing.
  * `ast.mli`, `printer.mli`: Concrete syntax tree and lossless pretty-printer.
  * `core_ir.mli`, `desugar.mli`, `normalize.mli`: Intermediate representation and desugaring.
  * `types.mli`, `typecheck.mli`, `unify.mli`: Hindley-Milner type inference.
  * `pattern.mli`, `exhaustiveness.mli`, `pat_compile.mli`: Pattern matching compiler.
  * `mod_ast.mli`, `namespace.mli`, `dep_graph.mli`: Modules and compilation units.
  * `opcode.mli`, `bytecode.mli`, `compiler.mli`, `vm.mli`: Stack bytecode VM.
  * `eval.mli`, `env.mli`, `value.mli`: Pure tree-walking reference interpreter.
  * `effect_runtime.mli`: Sandboxed capability-based effect runtime.
  * `stdlib/`: Host standard library modules (`List`, `Map`, `Option`, `Result`, `String`, `Math`).
  * `cli.mli`, `repl.mli`, `lsp_server.mli`: Tooling, REPL, and LSP server.
* `docs/`: Comprehensive technical documentation suite.
* `examples/`: Sample Vesper programs (`calculator.vesper`, `hello.vesper`, `fibonacci.vesper`, `factorial.vesper`).
* `tests/`: Test suites:
  * `tests/unit/`: Unit tests.
  * `tests/cram/`: End-to-end CLI snapshot tests.
  * `tests/laws/`: Continuous invariant regression suite verifying Laws 1–5.
* `verify.sh`: 4-layer automated verification script.

---

## 📄 License

This project is licensed under the [MIT License](LICENSE).
