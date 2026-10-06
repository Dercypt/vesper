# Vesper

A pure functional programming language implemented in OCaml, governed by formal invariants.

## Governance & Verification

All compiler passes and runtime semantics adhere to the 5 invariants defined in [LAWS.md](LAWS.md):
1. **Total Diagnosability**: User input never crashes the host runtime.
2. **Invertible Syntax**: `parse ∘ print ∘ parse = parse`.
3. **Strict Source Provenance**: AST and Core IR preserve source spans.
4. **Idempotent Normalization**: Desugaring is idempotent.
5. **Deterministic Evaluation**: Pure execution with fuel bounds.

* [LAWS.md](LAWS.md): Mathematical definitions of the 5 laws.
* [PRINCIPLES.md](PRINCIPLES.md): Architectural rules, `.mli` interfaces, warning policy.
* [HARNESS.md](HARNESS.md): 4-layer verification pipeline specification.
* [roadmap.md](roadmap.md): 10-phase engineering roadmap (Phases 1–10 complete).

Run the automated verification harness:
```bash
./verify.sh
```

## Quickstart

### Prerequisites (macOS / Linux)
```bash
opam switch create vesper 5.2.0
eval $(opam env)
opam install -y dune menhir ocamlformat alcotest
```

### Build & Run
```bash
# Execute a program
vesper run hello.vesper

# Check syntax and types without executing
vesper check calculator.vesper

# Format source code
vesper fmt --write hello.vesper

# Interactive REPL
vesper repl

# Disassemble bytecode
vesper disasm hello.vesper
```

## Calculator Demo

A working scientific calculator written in pure Vesper demonstrating curried functions, recursion, Newton-Raphson square roots, GCD/LCM, integer-to-string conversion, and ASCII UI rendering:

```bash
# Render ASCII calculator dashboard
./run_calculator.sh

# Interactive calculator prompt
./run_calculator.sh --interactive
```

## Language at a Glance

```vesper
// Declarations end with ';'
let x = 40 + 2;

// Curried functions
let double a = a * 2;

// Recursive functions
let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1);

// Anonymous functions
let add = fun a b -> a + b;

// Top-level expression evaluated and returned
double x;
```

## Documentation

* [Language Guide](docs/LANGUAGE_GUIDE.md): Syntax, operators, precedence, types, and inference.
* [Architecture](docs/ARCHITECTURE.md): Pipeline from lexer to bytecode VM and effects.
* [CLI Reference](docs/CLI_REFERENCE.md): Subcommands, flags, and exit codes.
* [Standard Library](docs/STANDARD_LIBRARY.md): Prelude combinators and core modules.
* [Calculator Showcase](docs/CALCULATOR_SHOWCASE.md): Walkthrough of `calculator.vesper`.
* [Self-Hosting Roadmap](docs/SELF_HOSTING.md): Bootstrapping path for Vesper in Vesper.

## License

[MIT](LICENSE)
