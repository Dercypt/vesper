# Vesper

A pure functional programming language with Hindley-Milner type inference, a bytecode virtual machine, and a built-in developer toolchain.

## Tech Stack

- OCaml
- Dune
- Menhir
- Alcotest
- OCamlFormat

## Features

- Hindley-Milner type inference
- Bytecode virtual machine
- Interactive REPL
- Code formatter
- Language Server Protocol

## Installation

Clone the repository and install dependencies with opam:

```bash
git clone https://github.com/Dercypt/vesper.git
cd vesper
opam install -y dune menhir alcotest ocamlformat
eval $(opam env)
dune build
```

> [!TIP]
> If your shell reports `command not found: dune`, the opam environment has not been activated in your current terminal session. Run `eval $(opam env)` to activate it, or add `eval $(opam env)` to your shell configuration (e.g., `~/.zshrc` or `~/.bashrc`) so that OCaml tools are permanently available in your `PATH`.

## Usage

Execute a Vesper program using Dune:

```bash
dune exec -- vesper run hello.vesper
```

## License

MIT
