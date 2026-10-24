Vesper

A pure functional programming language with Hindley-Milner type inference, a bytecode virtual machine, and a built-in developer toolchain.

Tech Stack

    OCaml

    Dune

    Menhir

    Alcotest

Features

    Hindley-Milner type inference

    Bytecode virtual machine

    Interactive REPL

    Code formatter

    Language Server Protocol

Installation
Bash

opam install -y dune menhir alcotest ocamlformat && dune build

Usage
Bash

dune exec -- vesper run hello.vesper
