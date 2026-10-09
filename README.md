# Vesper

**Vesper** is a pure, statically typed functional programming language implemented in OCaml, governed by formal mathematical invariants. It features Hindley-Milner type inference, a bytecode virtual machine, and a built-in toolchain including a REPL, code formatter, and Language Server Protocol (LSP) server.

---

## Key Features

- **Pure Functional Semantics**: Immutable bindings, curried functions, recursion, and expression-oriented syntax.
- **Hindley-Milner Type Inference**: Algorithm W infers principal types statically—no mandatory type annotations required.
- **Bytecode Virtual Machine**: Fast stack-based bytecode compiler and runtime VM with step/fuel-bounded execution.
- **Formally Verified Governance**: Complies with 5 sovereign domain laws guaranteeing total error diagnosability (no host crashes on user input), syntax invertibility, source span provenance, idempotent normalization, and determinism.
- **Complete Developer Toolchain**:
  - `vesper run` – Compile and execute bytecode
  - `vesper check` – Typecheck and validate without execution
  - `vesper fmt` – Deterministic, loss-free code formatter
  - `vesper repl` – Interactive read-eval-print loop
  - `vesper disasm` – Bytecode disassembler
  - `vesper lsp` – Language Server Protocol server for editor integration

---

## Language at a Glance

```vesper
// Declarations end with ';'
let message = "Hello, Vesper!";

// Curried functions
let add a b = a + b;

// Recursive functions
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1);

// Anonymous lambda expressions
let square = fun x -> x * x;

// Standard prelude utilities are available automatically
let clamped_val = clamp 0 100 (add 20 5);

// The trailing expression is evaluated and returned
factorial 5;
```

---

## Quickstart

### Prerequisites

- OCaml `>= 5.0`
- [opam](https://opam.ocaml.org/)
- Dune `>= 3.10`

Set up your switch and dependencies:

```bash
opam switch create vesper 5.2.0
eval $(opam env)
opam install -y dune menhir ocamlformat alcotest
```

### Build & Run

```bash
# Build the project
dune build

# Run a Vesper script via Dune
dune exec -- vesper run hello.vesper

# (Optional) Install the `vesper` binary globally into your opam switch
dune install
vesper run hello.vesper
```

---

## CLI Reference

| Command | Description |
| :--- | :--- |
| `vesper run <file>` | Typecheck, compile to bytecode, and execute |
| `vesper check <file>` | Parse and typecheck without executing |
| `vesper fmt [--write] <file>` | Pretty-print source code to stdout (or update file with `--write`) |
| `vesper repl` | Start an interactive REPL session |
| `vesper disasm <file>` | Display disassembled bytecode instructions |
| `vesper lsp` | Run the Language Server Protocol (LSP) server over stdio |
| `vesper -e "<expr>"` | Parse, typecheck, and validate inline source string |

---

## Interactive Demos

A scientific calculator written in pure Vesper (`calculator.vesper`) demonstrates curried operators, recursive Newton-Raphson square roots, GCD/LCM, integer-to-string conversion, and ASCII UI formatting:

```bash
# Display the ASCII calculator dashboard
./run_calculator.sh

# Launch an interactive calculator session
./run_calculator.sh --interactive
```

Additional examples are located in [`examples/`](examples/):
- `hello.vesper` – Basic countdown recursion
- `factorial.vesper` – Factorial computation
- `fibonacci.vesper` – Fibonacci sequence
- `calculator.vesper` – Scientific math and formatting engine

---

## Governance & Formal Laws

Every compiler pass and runtime evaluation conforms to the 5 sovereign laws defined in [`LAWS.md`](LAWS.md):

1. **Total Diagnosability**: Malformed user input produces diagnostic messages and never crashes the host runtime with uncaught exceptions.
2. **Invertible Syntax**: `parse ∘ print ∘ parse = parse`. Code formatting preserves AST identity.
3. **Strict Source Provenance**: All AST and Core IR nodes maintain precise source span tracking (`Ast.span`).
4. **Idempotent Normalization**: Desugaring and Core IR transformations are idempotent.
5. **Deterministic Evaluation**: Pure execution bounded by finite step/fuel counters.

Run the automated 4-layer verification harness:

```bash
./verify.sh
```

---

## Project Structure

```text
vesper-sandbox/
├── bin/          # CLI executable driver (`main.ml`)
├── lib/          # Core compiler pipeline and runtime
│   ├── lexer.mll # OCamllex scanner
│   ├── parser.mly# Menhir LALR(1) parser
│   ├── typecheck.ml # Hindley-Milner type inference (Algorithm W)
│   ├── desugar.ml# AST-to-Core IR desugaring
│   ├── compiler.ml # Bytecode compiler
│   ├── vm.ml     # Bytecode virtual machine
│   ├── repl.ml   # Interactive REPL
│   └── lsp_server.ml # Language Server Protocol implementation
├── examples/     # Example programs written in Vesper
├── tests/        # Unit tests, Cram CLI tests, and invariant law suites
│   └── laws/     # Formal invariant test suite
├── docs/         # In-depth architectural and language documentation
├── LAWS.md       # Formal mathematical definitions of the 5 laws
├── PRINCIPLES.md # Architectural guidelines and escalation rules
├── HARNESS.md    # Verification pipeline specifications
└── verify.sh     # End-to-end verification script
```

---

## Documentation

- [Language Guide](docs/LANGUAGE_GUIDE.md): Grammar, literals, operators, currying, recursion, types, and prelude.
- [Architecture](docs/ARCHITECTURE.md): Compiler pipeline, Core IR, typechecker, bytecode VM, and laws.
- [Standard Library](docs/STANDARD_LIBRARY.md): Prelude combinators and host stdlib modules.
- [CLI Reference](docs/CLI_REFERENCE.md): Subcommands (`run`, `check`, `fmt`, `repl`, `disasm`, `lsp`), flags, exit codes.
- [Calculator Showcase](docs/CALCULATOR_SHOWCASE.md): Scientific calculator walkthrough and architecture.
- [Self-Hosting Roadmap](docs/SELF_HOSTING.md): Core Vesper subset and bootstrap stages.

---

## License

This project is licensed under the [MIT License](LICENSE).
