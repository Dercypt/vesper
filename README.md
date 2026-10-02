# Vesper

A programming language built from scratch in OCaml.

## Architecture & Governance Harness

Vesper employs an invariant-first development harness. All contributions and compiler passes must comply with our domain invariants and engineering guidelines:

* [**`LAWS.md`**](file:///Users/zyndrex/vesper-sandbox/LAWS.md): The 5 non-negotiable domain invariants (diagnosability, invertible syntax, source provenance, idempotent lowering, deterministic evaluation).
* [**`PRINCIPLES.md`**](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md): Architectural decisions (ocamllex vs sedlex, warning policies), code defaults (`.mli`, immutability), and escalation triggers.
* [**`HARNESS.md`**](file:///Users/zyndrex/vesper-sandbox/HARNESS.md): Architecture of the verification pipeline.
* [**`AGENTS.md`**](file:///Users/zyndrex/vesper-sandbox/AGENTS.md): Operational protocols and rules of engagement for contributors and AI agents.
* [**`verify.sh`**](file:///Users/zyndrex/vesper-sandbox/verify.sh): Automated 4-layer verification pipeline (`dune fmt`, strict build, unit tests, invariant laws).

## Getting Started

### 1. Toolchain Setup (macOS)

```bash
brew install opam
opam init -y --bare
opam switch create vesper 5.2.0
eval $(opam env)
opam install -y dune menhir ocamlformat alcotest
```

### 2. Verify Repository

Run the automated governance verification harness:

```bash
./verify.sh
```

### 3. Project Structure

* `lib/`: Core compiler and runtime libraries (AST, parser, lexer, typechecker, evaluator).
* `bin/`: CLI driver and executable entry points.
* `tests/laws/`: Continuous invariant regression suite verifying [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md).
