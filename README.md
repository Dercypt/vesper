# Vesper

A pure, statically typed functional programming language implemented in OCaml with Hindley-Milner type inference, a bytecode virtual machine, and a built-in developer toolchain.

## Tech Stack

- **OCaml** (>= 5.0.0)
- **Dune** (>= 3.14)
- **Menhir & ocamllex**
- **Alcotest**
- **OCamlFormat**

## Features

- Pure functional semantics with immutable bindings, curried functions, and recursion.
- Hindley-Milner static type inference via Algorithm W.
- Stack-based bytecode compiler and virtual machine runtime.
- Integrated CLI toolchain providing execution, typechecking, formatting, a REPL, bytecode disassembly, and an LSP server.
- Formally verified invariants for syntax invertibility, error diagnosability, source spans, and determinism.

## Installation

Install dependencies and compile the project using opam and Dune:

```bash
opam install -y dune menhir alcotest ocamlformat
dune build
```

## Usage

Run Vesper commands using Dune:

```bash
# Execute a script
dune exec -- vesper run <file.vesper>

# Typecheck without executing
dune exec -- vesper check <file.vesper>

# Format a source file
dune exec -- vesper fmt <file.vesper>

# Start an interactive REPL session
dune exec -- vesper repl

# Disassemble bytecode
dune exec -- vesper disasm <file.vesper>

# Start the Language Server Protocol (LSP) server
dune exec -- vesper lsp
```

## License

This project is licensed under the MIT License.
