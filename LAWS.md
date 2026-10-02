# Vesper Domain Laws

The following laws represent **non-negotiable domain invariants** for the Vesper programming language compiler and runtime system. Every commit, refactor, and enhancement must uphold these invariants. Any pull request or change that violates a law will be rejected by the governance harness.

---

## Law 1: Total Diagnosability & Host Isolation (No Host Crashes)

> **"User errors must never crash the host compiler or runtime."**

* **Invariant**: Under no circumstances may user-supplied input (source code, CLI arguments, or runtime input) trigger an unhandled OCaml runtime exception (such as `Match_failure`, `Assert_failure`, `Failure`, `Invalid_argument`, or uncaught lexer/parser exceptions) at module boundaries.
* **Guarantee**: Every syntactically invalid, ill-typed, or dynamically failing program must be caught and converted into a structured `Result.Error` or a formal diagnostic object with an associated source span, returning a predictable exit code.
* **Verification**: Fuzzing and adversarial syntax tests in `tests/laws/` verify that random, malformed, or degenerate source streams terminate gracefully with structured diagnostics rather than panics or stack traces.

---

## Law 2: Invertible Concrete Syntax (Parse–Print Roundtrip)

> **"Printing a well-formed AST must yield syntax that reparses to an equivalent AST."**

* **Invariant**: For any syntactically valid AST $A$, formatting/pretty-printing $A$ to text and parsing it again must reproduce an AST equivalent to $A$ (modulo cosmetic whitespace and comments/trivia):
  $$\text{parse}(\text{print}(A)) \equiv A$$
* **Guarantee**: The language grammar and pretty-printer remain mutually consistent. Code generation, automated refactoring tools, and AST formatters will never corrupt or alter program semantics.
* **Verification**: Property-based roundtrip tests in `tests/laws/` generating arbitrary valid ASTs, formatting them to text, reparsing, and asserting structural equivalence.

---

## Law 3: Strict Source Provenance (Span Retention)

> **"Every node, intermediate representation, and diagnostic must retain source lineage."**

* **Invariant**: Every AST node, desugared intermediate representation (IR) node, and diagnostic artifact must carry a valid source span (`file`, `start_line`, `start_col`, `end_line`, `end_col`).
* **Guarantee**: Desugaring, optimization, and lowering passes must preserve or accurately derive child spans from their parent expressions. Synthesized or fabricated spans must explicitly record their provenance. Under no circumstances may a transformation drop location data into a null or detached state.
* **Verification**: AST and IR traversal tests in `tests/laws/` asserting that 100% of expressions in lowered representations possess non-degenerate, traceable source spans.

---

## Law 4: Idempotent Normalization & Desugaring

> **"Lowering passes must be idempotent and preserve semantic equivalence."**

* **Invariant**: Any normalization, canonicalization, or desugaring pass $\text{lower}$ must be idempotent:
  $$\text{lower}(\text{lower}(A)) \equiv \text{lower}(A)$$
* **Guarantee**: Repeated application of compiler transformations reaches a fixed point in a single step and does not introduce phase-ordering bugs or cascading mutation.
* **Verification**: Pass idempotency tests in `tests/laws/` running ASTs through repeated normalization steps and comparing output AST equality.

---

## Law 5: Deterministic Evaluation & Sound Termination

> **"Execution of a closed expression under a fixed environment must be fully deterministic."**

* **Invariant**: For any closed, valid Vesper program executed under an identical runtime environment and input stream, the interpreter or execution engine must produce the exact same sequence of transitions and final output across platforms.
* **Guarantee**: Language evaluation never relies on non-deterministic host language features (e.g., hash seed variations, unsequenced side-effects, or ambient host state) unless explicitly surfaced through designated I/O primitives.
* **Verification**: Determinism regression tests in `tests/laws/` evaluating programs across multiple runs and environments, asserting bit-for-bit output equivalence.
