# Vesper Principles & Escalation Triggers

This document governs engineering practices, default conventions, architectural decisions, and escalation triggers for the Vesper codebase.

---

## 1. Architectural Decisions & Toolchain Defaults

### Lexer: Ocamllex vs. Sedlex
* **Decision**: We use **ocamllex** for the bootstrap and initial phases of Vesper.
* **Rationale**: `ocamllex` is fast, zero-dependency, and built natively into Dune with zero PPX overhead. As long as Vesper syntax remains within standard ASCII and identifier boundaries, `ocamllex` keeps the bootstrap footprint lean and build times instantaneous.
* **Upgrade Trigger**: If and when Vesper requires native UTF-8 string literals or full Unicode identifier sets (e.g., UAX #31), we will evaluate either:
  1. A dedicated UTF-8 byte decoder layer atop `ocamllex`, or
  2. Migrating to `sedlex` (PPX-based Unicode lexer). Any such migration requires an architectural escalation.

### Compiler Warnings Policy
* **Flags**: Dune is configured globally with `(:standard -warn-error +A-4-9-27-44)`.
* **Warning Handling**:
  * Warnings are treated as fatal errors (`-warn-error +A`).
  * **Warning 4** (fragile pattern matching) is disabled to prevent brittle patterns from blocking forward development.
  * **Warning 9** (missing record fields in record pattern) is excluded from fatal errors so that record pattern matching during rapid prototyping does not artificially inflate boilerplate.
  * **Warning 27** (innocuous unused variable) is excluded from fatal errors so that pattern discards and prototype placeholders (`_x`) do not fail builds during early experimentation.
  * **Warning 44** (open statement shadows identifier) is excluded to allow ergonomic standard library opens.

---

## 2. Standard Escalation Triggers

The following changes represent architectural milestones that **must never be introduced implicitly**. Any agent or contributor must seek explicit review and user approval before proceeding:

1. **Dependency Escalations**:
   * Adding any new package to `dune-project` or `opam` dependencies (e.g., adding `cmdliner`, `ppx_deriving`, `sedlex`, or external C bindings).
2. **Grammar & AST Schema Escalations**:
   * Adding, modifying, or removing AST node definitions in `Ast.ml`.
   * Altering grammar tokens in `lexer.mll` or non-terminal production rules in `parser.mly`.
   * Changing the internal representation of source locations/spans.
3. **Public API & CLI Contract Changes**:
   * Modifying CLI flags, command-line arguments, or environment variables.
   * Changing public-facing JSON or structured diagnostic schemas.
   * Altering exported library interfaces (`.mli`) exposed to downstream consumers.
4. **Warning & Linter Suppressions**:
   * Inserting `[@warning "..."]` attributes in source code.
   * Modifying compiler flags in `dune-project` or `dune` to bypass warnings.

---

## 3. Engineering Code Defaults

All code in Vesper must adhere to the following defaults:

1. **Mandatory `.mli` Signatures**:
   * Every implementation file `foo.ml` must have a corresponding interface `foo.mli`.
   * Never leak internal implementation details or helper functions outside their module scope.
2. **Explicit Error Handling (Result over Exceptions)**:
   * Public module interfaces must never raise uncaught exceptions across module boundaries.
   * All fallible operations must return `('a, error) result`.
   * Lexer and parser exceptions (e.g., `MenhirLib.Error` or `Lexer.Error`) must be caught and converted to structured diagnostics at the boundary.
3. **Immutability First**:
   * Data structures within the compiler pipeline are immutable by default.
   * State manipulation across compiler passes is forbidden; passes must be pure functions ($T_1 \to T_2$).
4. **Formatting Compliance**:
   * All code must format cleanly with `dune fmt` without manual adjustments.
