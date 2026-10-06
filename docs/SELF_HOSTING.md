# Vesper Self-Hosting Blueprint

Goal: write the Vesper parser and typechecker in Vesper itself, bootstrapped by the OCaml implementation.

## Current language (what exists)

Literals (`int`, `bool`, `string`), unary/binary operators, `if`, `let` / `let rec` (curried parameters), `fun`, application, top-level `let` and expression declarations, a prelude of pure combinators (`id`, `compose`, `min`, `max`, ...).

## Minimum "Core Vesper" required for self-hosting

A compiler front end needs the following, in priority order. Items marked **(missing)** do not exist yet; each is an AST/grammar change and therefore an escalation under `PRINCIPLES.md` §2.2.

| Capability | Why the parser/typechecker needs it | Status |
|---|---|---|
| Closures, recursion, higher-order functions | Combinators, recursive descent | present |
| Integers, booleans, strings | Positions, flags, token text | present |
| String primitives (`length`, `get`/`sub`, `concat`, `of_int`) | Lexing | OCaml-side only (`Core_string`); **expose as runtime primitives** |
| Algebraic data types and `match` | Tokens, AST, types, errors | Pattern compiler exists (Phase 5); **surface syntax missing** |
| Lists and tuples | Token streams, substitutions | `Core_list`, ADT constructors exist; **surface syntax missing** |
| `option` / `result` | Total error handling (Law 1) | `Core_option`, `Core_result` OCaml-side; **expose** |
| Persistent maps | Type environments, substitutions | `Core_map` OCaml-side; **expose** |
| Records | AST nodes with spans | `VRecord` value exists; **surface syntax missing** |
| Modules / interfaces | Separate lexer, parser, typechecker units | Phase 7 module system |
| Capability-based I/O | Reading sources, writing diagnostics | `Effect_runtime` OCaml-side; **expose via prelude** |

## Bootstrap stages

1. **Stage 0** – OCaml implementation (this repository). Source of truth.
2. **Stage 1** – Vesper lexer (`lexer.vesper`): string scanning with `option`/`result`, emits a token ADT list. Validated by running both lexers over a fixture corpus and diffing token streams.
3. **Stage 2** – Vesper parser (`parser.vesper`): hand-written recursive descent / Pratt parser (Menhir grammar is *not* portable). Validated by `Law 2`: `parse ∘ print ∘ parse = parse` against Stage 0 on the corpus.
4. **Stage 3** – Vesper typechecker (`typecheck.vesper`): Algorithm W with persistent-map substitutions. Validated by comparing inferred schemes with Stage 0 on the corpus.
5. **Stage 4** – Fixed point: Stage N compiles Stage N's own source to bytecode identical to Stage N-1's output (determinism, Law 5).

## Invariants to preserve

- Every self-hosted pass returns `result`; no exceptions (Law 1).
- Spans are carried on every node (Law 3).
- Differential testing against Stage 0 gates every stage.
