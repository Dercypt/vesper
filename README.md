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
* `tests/cram/`: End-to-end CLI tests.
* `tests/laws/`: Continuous invariant regression suite verifying [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md).

## CLI Usage

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

Legacy forms `vesper <file>` and `vesper -e <source>` (parse and validate) remain supported.
Exit codes: `0` success, `1` diagnostics, `2` usage error.

REPL example:

```text
vesper> let x = 40 + 2;
val x : int = 42
vesper> x + 1;
- : int = 43
```

The LSP server supports `textDocument/didOpen`, `didChange`, `didClose` (publishing parse/type diagnostics) and `textDocument/formatting` (identical to `vesper fmt`).

## Language Guide

```text
let x = 40 + 2;                                  (* declarations end with ';' *)
let double a = a * 2;                            (* curried functions *)
let rec fact n = if n <= 1 then 1 else n * fact (n - 1);
let add = fun a b -> a + b;
double x;                                        (* expression declaration *)
```

Types: `int`, `bool`, `string`, functions, with Hindley-Milner inference. A prelude (`id`, `const`, `flip`, `compose`, `min`, `max`, `clamp`, `sign`, `succ`, `pred`, `not`, ...) is loaded automatically.

See [`docs/SELF_HOSTING.md`](docs/SELF_HOSTING.md) for the self-hosting roadmap.

## License

This project is licensed under the [MIT License](LICENSE).
