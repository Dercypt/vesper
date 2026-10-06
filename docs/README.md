# Vesper Documentation Portal

Welcome to the official documentation for **Vesper**, a pure functional programming language designed from first principles and implemented in OCaml.

Vesper is built with a formal governance model that guarantees five non-negotiable domain laws: total diagnosability (no host crashes), invertible concrete syntax, strict source provenance, idempotent desugaring, and deterministic evaluation.

---

## Documentation Index

| Guide | Description |
| :--- | :--- |
| [**Language Guide**](LANGUAGE_GUIDE.md) | Complete grammar, syntax, types, expressions, bindings, recursion, and prelude combinators. |
| [**Architecture & Compiler Internals**](ARCHITECTURE.md) | Deep dive into the 10 compiler phases: Lexer, Menhir parser, Core IR, Typechecker, ADTs, Modules, Bytecode VM, and sandboxed effects. |
| [**Standard Library & Prelude**](STANDARD_LIBRARY.md) | Full specification of the automatic prelude and host standard library modules (`List`, `Map`, `Option`, `Result`, `String`, `Math`). |
| [**CLI & Tooling Reference**](CLI_REFERENCE.md) | Command-line manual for `vesper run`, `check`, `fmt`, `repl`, `disasm`, `lsp`, and environment flags. |
| [**Scientific Calculator Showcase**](CALCULATOR_SHOWCASE.md) | In-depth walkthrough of the pure Vesper terminal scientific calculator (`calculator.vesper` and `run_calculator.sh`). |
| [**Self-Hosting Roadmap**](SELF_HOSTING.md) | Blueprint for bootstrapping Vesper within Vesper, defining Core Vesper and bootstrap stages 0–4. |
| [**Domain Laws & Invariants**](../LAWS.md) | The 5 sovereign mathematical laws governing all compiler passes and runtime semantics. |
| [**Engineering Principles**](../PRINCIPLES.md) | Architectural invariants, code standards (`.mli` first, zero warnings), and escalation triggers. |
| [**Verification Harness**](../HARNESS.md) | The 4-layer automated verification pipeline (`./verify.sh`). |
| [**Agent Governance**](../AGENTS.md) | Operating protocols and contributor guidelines for AI and human engineers. |

---

## Quick Tour: Running Vesper

### 1. Build and Run the Scientific Calculator

Run the interactive ASCII scientific calculator written purely in Vesper:

```bash
# Display the terminal dashboard
./run_calculator.sh

# Or start the live interactive calculator session
./run_calculator.sh --interactive
```

### 2. Basic Compiler Commands

```bash
# Typecheck, compile to bytecode, and execute
vesper run hello.vesper

# Parse and typecheck without executing
vesper check calculator.vesper

# Format source code losslessly
vesper fmt calculator.vesper

# Launch the interactive REPL
vesper repl

# Disassemble bytecode instructions with source-span mappings
vesper disasm calculator.vesper
```

---

## Governance & Verification

All code in Vesper must satisfy the four-layer verification harness before merging:

```bash
./verify.sh
```

1. **Layer 1 (Formatting)**: `dune build @fmt` ensures zero formatting divergence.
2. **Layer 2 (Strict Compilation)**: `dune build @all` with `-warn-error +A-4-9-27-44` enforces zero warnings.
3. **Layer 3 (Unit & Integration Tests)**: `dune runtest` executes Cram and unit tests.
4. **Layer 4 (Law Verification)**: `tests/laws/` property suites mathematically verify Laws 1–5.
