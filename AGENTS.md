# Vesper Agent Governance Guidelines

This document establishes the operating protocol for AI agents and human contributors working within the Vesper codebase.

---

## 1. Core Operating Mandates

Every contributor and agent must obey these non-negotiable rules:

1. **Laws are Sovereign**:
   The domain invariants in [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md) are strictly non-negotiable. Code that violates these invariants (such as uncaught host exceptions on user input, or loss of source span tracking) must not be merged.
2. **Escalation Triggers Require Explicit Sign-Off**:
   Consult [`PRINCIPLES.md`](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md) before making architectural changes. If your task encounters an escalation trigger (e.g., adding an `opam` package, altering `Ast.ml` node types, modifying `parser.mly`/`lexer.mll`, or adding warning suppressions), you must stop and request explicit user confirmation.
3. **Always Run the Harness**:
   Before declaring any task or turn complete, execute [`./verify.sh`](file:///Users/zyndrex/vesper-sandbox/verify.sh) (or `dune build @fmt && dune build @all && dune runtest`). Never present broken, unformatted, or failing code.
4. **Interfaces Before Implementations**:
   Every new module in `lib/` must have a matching `.mli` interface file. Never expose internal helpers or raw exceptions.
5. **No Silent Suppressions**:
   Do not insert `[@warning "..."]` attributes or disable compiler warnings to force a build to pass. Address the underlying type error or pattern exhaustiveness issue directly.

---

## 2. Standard Workflow for Feature Development

When implementing a new feature or compiler pass:

1. **Check Invariant Compatibility**: Review [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md) to ensure your design preserves invertibility, source spans, and determinism.
2. **Define Signatures First**: Write or update `.mli` files to declare pure types and `Result.t` error variants.
3. **Implement Pure Transformations**: Implement the logic in `.ml` files using immutable data structures and total pattern matching.
4. **Add Unit & Law Tests**:
   * Add feature unit tests in `tests/`.
   * Add or extend domain invariant tests in [`tests/laws/`](file:///Users/zyndrex/vesper-sandbox/tests/laws/).
5. **Format & Verify**:
   * Run `dune fmt` to format code.
   * Run `./verify.sh` to ensure all four layers of the harness pass.

---

## 3. Lexer & Grammar Evolution Protocol

* Vesper uses **ocamllex** by default to maintain zero-overhead, fast builds.
* Do not introduce `sedlex` or complex PPX dependencies without an approved escalation.
* If Unicode support or native UTF-8 literals are required, formulate an escalation proposal describing the decoder design or PPX dependency before implementation.
