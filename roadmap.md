# Vesper 10-Phase Engineering Roadmap

This document outlines the master architectural roadmap for **Vesper**, a programming language engineered from scratch in OCaml. The development of Vesper is anchored in the non-negotiable domain invariants specified in [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md), the architectural guidelines defined in [`PRINCIPLES.md`](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md), and the continuous automated verification harness detailed in [`HARNESS.md`](file:///Users/zyndrex/vesper-sandbox/HARNESS.md).

---

## Roadmap Overview & Phase Matrix

| Phase | Focus Area | Primary Domain Law | Key Milestone | Status |
| :--- | :--- | :--- | :--- | :---: |
| **Phase 1** | Concrete Syntax & Invertible Parsing | [**Law 2** (Invertible Syntax)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-2-invertible-concrete-syntax-parseprint-roundtrip) | Lexer, Menhir Parser, Canonical Printer, Roundtrip Suite | ✅ **COMPLETE** |
| **Phase 2** | Diagnostics & Host Isolation | [**Law 1** (Total Diagnosability)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) | Resilient Syntax Recovery, Pretty Diagnostics, Zero Host Panics | ✅ **COMPLETE** |
| **Phase 3** | Core IR, Provenance & Desugaring | [**Law 3** (Span Lineage)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention) & [**Law 4** (Idempotence)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-4-idempotent-normalization--desugaring) | Explicit Core IR, Desugaring Passes, 100% Provenance Retention | ✅ **COMPLETE** |
| **Phase 4** | Deterministic Tree-Walking Runtime | [**Law 5** (Deterministic Evaluation)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination) | Pure Value Model, Environment Scoping, Step/Fuel Bounds | ✅ **COMPLETE** |
| **Phase 5** | Static Analysis & Type Checking | [**Law 1** (Diagnosability)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 3** (Provenance)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention) | Bidirectional Type Checking, Hindley-Milner Inference, Type Errors | ✅ **COMPLETE** |
| **Phase 6** | Algebraic Data Types & Pattern Matching | [**Law 1** (Diagnosability)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 4** (Idempotence)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-4-idempotent-normalization--desugaring) | Sum & Product Types, Maranget Exhaustiveness Checking, Decision Trees | ✅ **COMPLETE** |
| **Phase 7** | Module System & Compilation Units | [**Law 3** (Provenance)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention) & [**Law 5** (Determinism)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination) | Namespaces, Interface Checking, Dependency Graph & Cycle Detection | ✅ **COMPLETE** |
| **Phase 8** | Bytecode Virtual Machine | [**Law 5** (Deterministic Evaluation)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination) | Compact Stack VM, Instruction Set Architecture, Bytecode Compiler | ✅ **COMPLETE** |
| **Phase 9** | Standard Library & Sandboxed Effects | [**Law 1** (Diagnosability)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 5** (Determinism)](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination) | Pure Core Stdlib, Sandboxed Effect Runtime, Replayable I/O Boundary | ✅ **COMPLETE** |
| **Phase 10** | Tooling Ecosystem & Production CLI | All Laws ([**1–5**](file:///Users/zyndrex/vesper-sandbox/LAWS.md)) | Interactive REPL, Formatter CLI, LSP Server Foundation, Self-Hosting Path | ✅ **COMPLETE** |

---

## Phase 1: Concrete Syntax, Lexing, Menhir Parsing & Invertible Pretty-Printing

### Objective
Establish the formal concrete grammar for Vesper, construct an efficient `ocamllex` scanner and a robust `menhir` LALR(1) parser, implement a lossless AST pretty-printer, and prove [**Law 2 (Invertible Concrete Syntax)**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-2-invertible-concrete-syntax-parseprint-roundtrip) through automated roundtrip property tests.

### Governance & Escalation Triggers
* **Parser/Lexer Escalation**: Modifying `parser.mly`, `lexer.mll`, and expanding `Ast.ml` node types is expected in this phase and requires explicit review against [`PRINCIPLES.md`](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md#2-standard-escalation-triggers).
* **Dependencies**: Native `ocamllex` and `menhir` are permitted. No PPX dependencies or `sedlex` may be introduced without sign-off.

### Deliverables
* [`lib/tokens.mli`](file:///Users/zyndrex/vesper-sandbox/lib/tokens.mli)
* [`lib/lexer.mll`](file:///Users/zyndrex/vesper-sandbox/lib/lexer.mll)
* [`lib/parser.mly`](file:///Users/zyndrex/vesper-sandbox/lib/parser.mly)
* [`lib/ast.mli`](file:///Users/zyndrex/vesper-sandbox/lib/ast.mli) / [`lib/ast.ml`](file:///Users/zyndrex/vesper-sandbox/lib/ast.ml) (expanded expression & declaration nodes)
* [`lib/printer.mli`](file:///Users/zyndrex/vesper-sandbox/lib/printer.mli) / [`lib/printer.ml`](file:///Users/zyndrex/vesper-sandbox/lib/printer.ml)
* [`lib/parse_facade.mli`](file:///Users/zyndrex/vesper-sandbox/lib/parse_facade.mli) / [`lib/parse_facade.ml`](file:///Users/zyndrex/vesper-sandbox/lib/parse_facade.ml)
* [`tests/laws/test_law2_roundtrip.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_law2_roundtrip.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 1.1: Token Definitions & `ocamllex` Scanner
Implement zero-overhead lexical scanning with precise byte and line/column coordinate tracking.

* **Step 1.1.1**: Define canonical tokens in `lib/parser.mly` (literals: integers, booleans, strings; keywords: `let`, `in`, `if`, `then`, `else`, `fun`, `match`, `with`; punctuation: parens, braces, operators, colons, arrows).
* **Step 1.1.2**: Write `lib/lexer.mll` using standard OCaml lexing conventions with automatic newline incrementation (`Lexing.new_line lexbuf`).
* **Step 1.1.3**: Implement strict character escapes (`\n`, `\t`, `\\`, `\"`) in string literal scanning without throwing unhandled host exceptions.
* **Step 1.1.4**: Wrap lexer entry point in `lib/parse_facade.ml` catching `Lexer.Error` and converting into `(token, Ast.error) result`.
* **Step 1.1.5**: Run `dune build @all` to verify zero compiler warnings and verify clean lexer generation.

#### Mini-Goal 1.2: LALR(1) Menhir Grammar & Concrete Parsing
Formalize grammar productions for expressions, bindings, and declarations with explicit operator precedence.

* **Step 1.2.1**: Define operator associativity and precedence levels in `lib/parser.mly` (`%left`, `%right`, `%nonassoc`).
* **Step 1.2.2**: Implement production rules for atomic expressions, function calls, arithmetic/logical binary operations, conditional branches, and let-bindings.
* **Step 1.2.3**: Embed source span synthesis in every grammar rule using `$loc` / Menhir position markers into `Ast.span`.
* **Step 1.2.4**: Create `lib/parse_facade.mli` exposing pure entry points:
  ```ocaml
  val parse_string : file:string -> string -> (Ast.program, Ast.error) result
  ```
* **Step 1.2.5**: Ensure Menhir generates zero shift/reduce and reduce/reduce conflicts (`--explain` dune flag configured).

#### Mini-Goal 1.3: Lossless AST Pretty-Printer
Construct a deterministic pretty-printer translating AST trees back into canonical, human-readable Vesper source text.

* **Step 1.3.1**: Create `lib/printer.mli` exposing `val to_string : Ast.program -> string` and `val expr_to_string : Ast.expr -> string`.
* **Step 1.3.2**: Implement precedence-aware parenthesization in `lib/printer.ml` to eliminate unnecessary parentheses while preserving semantics.
* **Step 1.3.3**: Ensure formatting adheres to predictable indentation (2 spaces) and standardized binary operator spacing.
* **Step 1.3.4**: Unit-test expression stringification across all literal, variable, application, and conditional forms.

#### Mini-Goal 1.4: Invertible Parse-Print Roundtrip Suite ([Law 2 Verification](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-2-invertible-concrete-syntax-parseprint-roundtrip))
Prove that $\text{parse}(\text{print}(A)) \equiv A$ for all well-formed syntax trees.

* **Step 1.4.1**: Author property generator in `tests/laws/test_law2_roundtrip.ml` producing syntactically valid random ASTs.
* **Step 1.4.2**: Implement structural equivalence equality function `val ast_equal : Ast.program -> Ast.program -> bool` (comparing tree nodes modulo source spans).
* **Step 1.4.3**: Execute roundtrip tests across 1,000 generated programs: parse generated code, print to text, parse again, and assert structural equality.
* **Step 1.4.4**: Register test suite in `tests/laws/dune` and run `./verify.sh`.

---

## Phase 2: Total Diagnosability, Error Recovery & Host Crash Elimination

### Objective
Enforce [**Law 1 (Total Diagnosability & Host Isolation)**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes). Ensure that no user-supplied input—regardless of syntax corruption, malformed bytes, or truncated tokens—can ever crash the host OCaml runtime or leak unhandled exceptions.

### Governance & Escalation Triggers
* **Error Representation**: Structured diagnostics must retain span provenance ([**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention)).
* **CLI Contract**: CLI must return standardized nonzero exit codes for compilation errors without stack traces.

### Deliverables
* [`lib/diagnostic.mli`](file:///Users/zyndrex/vesper-sandbox/lib/diagnostic.mli) / [`lib/diagnostic.ml`](file:///Users/zyndrex/vesper-sandbox/lib/diagnostic.ml)
* [`lib/error_code.mli`](file:///Users/zyndrex/vesper-sandbox/lib/error_code.mli) / [`lib/error_code.ml`](file:///Users/zyndrex/vesper-sandbox/lib/error_code.ml)
* Menhir error message catalog (`lib/parser_messages.messages`)
* [`tests/laws/test_law1_diagnosability.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_law1_diagnosability.ml)
* Adversarial input fuzzing harness in `tests/`

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 2.1: Structured Diagnostic Data Model & Terminal Renderer
Design a compiler diagnostic engine featuring source line snippets, caret pointers, error codes, and contextual help messages.

* **Step 2.1.1**: Define structured diagnostic types in `lib/diagnostic.mli`:
  ```ocaml
  type severity = Error | Warning | Note
  type t = {
    code : Error_code.t;
    severity : severity;
    span : Ast.span;
    message : string;
    hint : string option;
  }
  ```
* **Step 2.1.2**: Implement source snippet extractor reading source text lines by 1-indexed line numbers without reading outside buffer bounds.
* **Step 2.1.3**: Implement ANSI-colored terminal formatter with caret underline pointers (`^^^`) beneath the erroneous column range.
* **Step 2.1.4**: Add plain-text fallbacks for non-TTY stdout/stderr outputs.

#### Mini-Goal 2.2: Menhir Error Recovery & Syntax Diagnostics
Replace raw syntax error panics with descriptive syntax error messages.

* **Step 2.2.1**: Configure Menhir's table-based error message inspection engine via `--compile-errors`.
* **Step 2.2.2**: Maintain `parser_messages.messages` mapping parser state IDs to human-readable explanations (e.g., missing semicolons, unclosed parentheses, missing `in` keyword).
* **Step 2.2.3**: Intercept Menhir error tokens and convert parser failure states into structured `Diagnostic.t` records.
* **Step 2.2.4**: Implement lexer error resilience: unterminated strings, illegal characters, and integer overflows caught cleanly with diagnostic spans.

#### Mini-Goal 2.3: Graceful CLI Exit Boundary
Guarantee complete host isolation at the process boundary.

* **Step 2.3.1**: Create `lib/driver.mli` providing a safe evaluation wrapper that catches any accidental OCaml runtime exceptions (`Sys_error`, `End_of_file`, etc.) and transforms them into diagnostic errors.
* **Step 2.3.2**: Update `bin/main.ml` to invoke the driver, print rendered diagnostics to `stderr`, and exit with code `1` on error, never raising an unhandled exception.
* **Step 2.3.3**: Ensure zero stack trace leakage under any user error.

#### Mini-Goal 2.4: Adversarial Fuzzing & Law 1 Invariant Verification
Stress-test parser and lexer against hostile, chaotic, and truncated input streams.

* **Step 2.4.1**: Write adversarial generator in `tests/laws/test_law1_diagnosability.ml` producing null bytes, unbalanced quotes, deep nesting (10,000+ parens), and random bit streams.
* **Step 2.4.2**: Feed 10,000 adversarial samples through `Parse_facade.parse_string`.
* **Step 2.4.3**: Assert that 100% of adversarial runs terminate with `Result.Error { span; _ }` where `Ast.span_is_valid span` is true.
* **Step 2.4.4**: Verify that zero host exceptions escape to the test runner. Execute `./verify.sh`.

---

## Phase 3: Core Intermediate Representation (Core IR), Desugaring & Normalization

### Objective
Construct a minimal, desugared intermediate representation (Core IR), implement lowering passes that preserve 100% source span lineage ([**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention)), and ensure normalization passes achieve idempotent fixed-points ([**Law 4**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-4-idempotent-normalization--desugaring)).

### Governance & Escalation Triggers
* **IR Node Escalation**: Defining Core IR node types is an architectural milestone. Keep representations pure and immutable.
* **Provenance Invariant**: Fabricated or synthetic nodes created during desugaring must carry derived provenance spans referencing their source origin.

### Deliverables
* [`lib/core_ir.mli`](file:///Users/zyndrex/vesper-sandbox/lib/core_ir.mli) / [`lib/core_ir.ml`](file:///Users/zyndrex/vesper-sandbox/lib/core_ir.ml)
* [`lib/desugar.mli`](file:///Users/zyndrex/vesper-sandbox/lib/desugar.mli) / [`lib/desugar.ml`](file:///Users/zyndrex/vesper-sandbox/lib/desugar.ml)
* [`lib/normalize.mli`](file:///Users/zyndrex/vesper-sandbox/lib/normalize.mli) / [`lib/normalize.ml`](file:///Users/zyndrex/vesper-sandbox/lib/normalize.ml)
* [`tests/laws/test_law3_provenance.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_law3_provenance.ml)
* [`tests/laws/test_law4_idempotence.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_law4_idempotence.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 3.1: Explicit Core IR Definition
Specify an explicit, simplified intermediate representation tailored for analysis and evaluation.

* **Step 3.1.1**: Define `Core_ir.t` containing atomic primitives: literals, variable bindings (`let`), lambda abstractions (`fun`), applications (`app`), primitive operators (`primop`), and explicit conditional branching.
* **Step 3.1.2**: Mandate that every `Core_ir.expr` node wraps an `Ast.span` alongside its descriptor:
  ```ocaml
  type expr = { span : Ast.span; desc : desc }
  ```
* **Step 3.1.3**: Provide structural equality and validation functions in `lib/core_ir.mli`.

#### Mini-Goal 3.2: Lowering & Desugaring Engine ([Law 3 Span Lineage](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention))
Transform surface AST constructs into canonical Core IR without losing location data.

* **Step 3.2.1**: Implement `Desugar.lower_expr : Ast.expr -> (Core_ir.expr, Ast.error) result`.
* **Step 3.2.2**: Desugar multi-argument functions into curried single-argument lambdas while assigning the exact span of the parameter list to intermediate lambda nodes.
* **Step 3.2.3**: Desugar compound operators (e.g., `e1 += e2`, `e1 && e2`) into explicit primitive operations or conditional expressions, deriving sub-spans directly from operand spans.
* **Step 3.2.4**: Verify that synthesized spans are never initialized with `dummy_span` if any parent or constituent span exists.

#### Mini-Goal 3.3: Idempotent Normalization Passes ([Law 4 Verification](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-4-idempotent-normalization--desugaring))
Implement simplification passes that achieve fixed-point convergence in a single step.

* **Step 3.3.1**: Implement constant folding for basic arithmetic and boolean expressions in `lib/normalize.ml`.
* **Step 3.3.2**: Implement dead code elimination for unreachable branches in `if true then e1 else e2`.
* **Step 3.3.3**: Ensure simplification preserves source spans of surviving sub-expressions.
* **Step 3.3.4**: Prove mathematically and empirically that $\text{normalize}(\text{normalize}(e)) \equiv \text{normalize}(e)$.

#### Mini-Goal 3.4: Provenance & Idempotence Regression Suites
Validate Laws 3 and 4 across arbitrary programs.

* **Step 3.4.1**: In `tests/laws/test_law3_provenance.ml`, implement an AST traversal verifying that every node in desugared Core IR satisfies `Ast.span_is_valid`.
* **Step 3.4.2**: In `tests/laws/test_law4_idempotence.ml`, run 1,000 programs through `Desugar.lower` and `Normalize.normalize`.
* **Step 3.4.3**: Assert that running `Normalize.normalize` a second time produces an identical Core IR AST.
* **Step 3.4.4**: Run `./verify.sh` to confirm all law checks pass.

---

## Phase 4: Deterministic Tree-Walking Runtime & Pure Evaluation

### Objective
Implement an environment-based tree-walking interpreter executing Core IR expressions with mathematical determinism, bounded execution steps, and zero unhandled host runtime exceptions ([**Law 5**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination)).

### Governance & Escalation Triggers
* **State Management**: Runtime state must be purely functional. Mutability is forbidden across interpreter step boundaries.
* **Determinism Guarantee**: No reliance on host hashing seeds, thread scheduling, or ambient clock/environment variables.

### Deliverables
* [`lib/value.mli`](file:///Users/zyndrex/vesper-sandbox/lib/value.mli) / [`lib/value.ml`](file:///Users/zyndrex/vesper-sandbox/lib/value.ml)
* [`lib/env.mli`](file:///Users/zyndrex/vesper-sandbox/lib/env.mli) / [`lib/env.ml`](file:///Users/zyndrex/vesper-sandbox/lib/env.ml)
* [`lib/eval.mli`](file:///Users/zyndrex/vesper-sandbox/lib/eval.mli) / [`lib/eval.ml`](file:///Users/zyndrex/vesper-sandbox/lib/eval.ml)
* [`tests/laws/test_law5_determinism.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_law5_determinism.ml)
* Unit tests for arithmetic, closures, recursion, and scope shadowing

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 4.1: Runtime Values & Immutable Environments
Formulate pure representations for evaluation values and lexical environments.

* **Step 4.1.1**: Define runtime values in `lib/value.mli`:
  ```ocaml
  type t =
    | VInt of int
    | VBool of bool
    | VString of string
    | VClosure of { param : string; body : Core_ir.expr; env : env }
    | VUnit
  and env
  ```
* **Step 4.1.2**: Implement `lib/env.ml` backed by an immutable `Map.Make(String)` or persistent association list.
* **Step 4.1.3**: Provide pure environment operations: `empty`, `extend`, `lookup`, and `shadow`.
* **Step 4.1.4**: Define runtime errors with source spans: unbound variable, division by zero, type mismatch at runtime.

#### Mini-Goal 4.2: Pure Expression Evaluator
Write a total evaluation function using pure step reductions.

* **Step 4.2.1**: Implement `Eval.eval_expr : env -> Core_ir.expr -> (Value.t, Ast.error) result`.
* **Step 4.2.2**: Implement evaluation for literals and variable lookups.
* **Step 4.2.3**: Implement binary operators (`+`, `-`, `*`, `/`, `==`, `<`, `&&`, `||`) with safe division checking to prevent host `Division_by_zero` exceptions.
* **Step 4.2.4**: Implement lambda creation (capturing lexical environment into `VClosure`) and function application (evaluating arguments and evaluating closure body in extended environment).
* **Step 4.2.5**: Implement recursive bindings using explicit fixpoint or environment cycles wrapped in pure records.

#### Mini-Goal 4.3: Fuel / Step Accounting for Termination Safety
Equip the interpreter with execution bounds to prevent infinite loops from hanging the host harness.

* **Step 4.3.1**: Add `fuel : int` parameter or step counter configuration to evaluator.
* **Step 4.3.2**: Decrement fuel on function applications and loop transitions; return `Error { span; message = "Execution budget exhausted" }` when fuel reaches zero.
* **Step 4.3.3**: Expose configurable default fuel in `lib/eval.mli` for test suites and CLI runs.

#### Mini-Goal 4.4: Determinism Invariant Suite ([Law 5 Verification](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination))
Prove evaluation determinism across repeated executions.

* **Step 4.4.1**: In `tests/laws/test_law5_determinism.ml`, run 100 complex programs (recursive fibonacci, closures, nested let-bindings) across 10 iterations each.
* **Step 4.4.2**: Record output values, step counts, and error states.
* **Step 4.4.3**: Assert bit-for-bit equivalence across all runs under identical inputs.
* **Step 4.4.4**: Verify that uncaught host exceptions are completely absent. Run `./verify.sh`.

---

## Phase 5: Static Analysis, Bidirectional Type Checking & Type Inference

### Objective
Introduce a static type system for Vesper. Implement bidirectional type checking combined with Hindley-Milner type inference, delivering detailed type error diagnostics with source span precision ([**Law 1**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention)).

### Governance & Escalation Triggers
* **AST Type Annotations**: Adding type annotation syntax to AST nodes is a grammar escalation trigger requiring sign-off.
* **Compiler Passes**: The typechecker must remain a pure pass returning `(typed_ir, Diagnostic.t list) result`.

### Deliverables
* [`lib/types.mli`](file:///Users/zyndrex/vesper-sandbox/lib/types.mli) / [`lib/types.ml`](file:///Users/zyndrex/vesper-sandbox/lib/types.ml)
* [`lib/type_env.mli`](file:///Users/zyndrex/vesper-sandbox/lib/type_env.mli) / [`lib/type_env.ml`](file:///Users/zyndrex/vesper-sandbox/lib/type_env.ml)
* [`lib/unify.mli`](file:///Users/zyndrex/vesper-sandbox/lib/unify.mli) / [`lib/unify.ml`](file:///Users/zyndrex/vesper-sandbox/lib/unify.ml)
* [`lib/typecheck.mli`](file:///Users/zyndrex/vesper-sandbox/lib/typecheck.mli) / [`lib/typecheck.ml`](file:///Users/zyndrex/vesper-sandbox/lib/typecheck.ml)
* [`tests/unit/test_typechecker.ml`](file:///Users/zyndrex/vesper-sandbox/tests/unit/test_typechecker.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 5.1: Type Algebra & Type Representation
Define pure types, type variables, and substitution mappings.

* **Step 5.1.1**: Define types in `lib/types.mli`:
  ```ocaml
  type t =
    | TyInt
    | TyBool
    | TyString
    | TyUnit
    | TyArrow of t * t
    | TyVar of int
  ```
* **Step 5.1.2**: Implement substitution map `type subst = t IntMap.t` and type variable substitution functions `val apply : subst -> t -> t`.
* **Step 5.1.3**: Implement free type variable calculation `val ftv : t -> IntSet.t` for generalization.
* **Step 5.1.4**: Define type schemes `type scheme = Forall of int list * t`.

#### Mini-Goal 5.2: Hindley-Milner Unification & Occurs Check
Implement pure unification with informative failure reporting.

* **Step 5.2.1**: Implement `val unify : span:Ast.span -> Types.t -> Types.t -> (Types.subst, Ast.error) result`.
* **Step 5.2.2**: Implement occurs check (`occurs_check : int -> Types.t -> bool`) to detect infinite recursive types (e.g., $\tau = \tau \to \text{int}$).
* **Step 5.2.3**: Format unification error diagnostics detailing expected vs. actual types alongside the offending expression span.

#### Mini-Goal 5.3: Bidirectional Type Checking Engine
Combine type inference (synthesis) with type verification (checking) for expressions.

* **Step 5.3.1**: Implement `infer : Type_env.t -> Ast.expr -> (Types.subst * Types.t, Ast.error) result`.
* **Step 5.3.2**: Implement `check : Type_env.t -> Ast.expr -> Types.t -> (Types.subst, Ast.error) result`.
* **Step 5.3.3**: Support explicit type annotations: `(e : T)` triggers checking mode, guiding type inference.
* **Step 5.3.4**: Implement let-polymorphism: generalize type variables not free in the typing environment upon let-binding completion.

#### Mini-Goal 5.4: Type Diagnostic Verification Suite
Test positive type validity and negative diagnostic clarity.

* **Step 5.4.1**: Author unit tests verifying correct typing of polymorphic identity, Church numerals, and higher-order functions in `tests/unit/test_typechecker.ml`.
* **Step 5.4.2**: Test negative cases: type mismatch, unbound variables, occurs check violations, verifying precise spans in errors.
* **Step 5.4.3**: Assert that no type error generates an unhandled exception.
* **Step 5.4.4**: Run `./verify.sh` to confirm all formatting and test assertions pass.

---

## Phase 6: Algebraic Data Types, Structs & Exhaustive Pattern Matching

### Objective
Expand the language syntax and type system with user-defined Algebraic Data Types (ADTs) and records. Implement Maranget's matrix-based exhaustiveness and redundancy algorithm to guarantee pattern matching safety and eliminate match failures ([**Law 1**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 4**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-4-idempotent-normalization--desugaring)).

### Governance & Escalation Triggers
* **AST Extension**: Adding `type` declarations, constructors, record syntax, and pattern nodes requires grammar escalation approval.
* **Warning Policy**: Missing pattern match warnings must be surfaced as diagnostic errors or structured warnings without compiler bypasses.

### Deliverables
* [`lib/pattern.mli`](file:///Users/zyndrex/vesper-sandbox/lib/pattern.mli) / [`lib/pattern.ml`](file:///Users/zyndrex/vesper-sandbox/lib/pattern.ml)
* [`lib/exhaustiveness.mli`](file:///Users/zyndrex/vesper-sandbox/lib/exhaustiveness.mli) / [`lib/exhaustiveness.ml`](file:///Users/zyndrex/vesper-sandbox/lib/exhaustiveness.ml)
* [`lib/pat_compile.mli`](file:///Users/zyndrex/vesper-sandbox/lib/pat_compile.mli) / [`lib/pat_compile.ml`](file:///Users/zyndrex/vesper-sandbox/lib/pat_compile.ml)
* [`tests/unit/test_patterns.ml`](file:///Users/zyndrex/vesper-sandbox/tests/unit/test_patterns.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 6.1: Grammar & AST Support for ADTs and Patterns
Introduce sum types, records, and pattern matching syntax into the AST and parser.

* **Step 6.1.1**: Define ADT declarations in `Ast.mli`:
  ```ocaml
  type type_decl = {
    name : string;
    params : string list;
    variants : (string * Types.t list) list;
  }
  ```
* **Step 6.1.2**: Define pattern nodes in `Ast.mli`: `PWildcard`, `PVar`, `PLit`, `PConstructor`, `PRecord`.
* **Step 6.1.3**: Update `lib/parser.mly` and `lib/lexer.mll` with `type`, `|`, `match`, `with`, `{`, `}` tokens.
* **Step 6.1.4**: Extend `lib/printer.ml` to format pattern matching blocks and ADT declarations, updating roundtrip tests.

#### Mini-Goal 6.2: Maranget's Matrix Exhaustiveness & Redundancy Checking
Implement pattern exhaustiveness checking via Luc Maranget's matrix compilation algorithm.

* **Step 6.2.1**: Represent pattern clauses as a pattern matrix $P \times Q$ in `lib/exhaustiveness.ml`.
* **Step 6.2.2**: Implement the usefulness algorithm checking whether a pattern vector is useful with respect to the existing matrix.
* **Step 6.2.3**: Detect non-exhaustive matches: synthesize a witness pattern showing an unmatched case and report as `Diagnostic.t` with span.
* **Step 6.2.4**: Detect redundant (unreachable) pattern clauses and emit structured dead-code warnings with source spans.

#### Mini-Goal 6.3: Compiling Pattern Matches into Efficient Decision Trees
Lower high-level pattern matching into primitive switch tests and variable bindings in Core IR.

* **Step 6.3.1**: Define decision tree IR in `lib/pat_compile.mli`: `Leaf`, `SwitchConstructor`, `SwitchLit`, `Failure`.
* **Step 6.3.2**: Implement matrix decomposition lowering pattern matrices into binary/multiway decision trees.
* **Step 6.3.3**: Retain source spans of matched variables and constructor patterns throughout lowering ([**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention)).
* **Step 6.3.4**: Integrate pattern compilation into `Desugar.lower_expr`.

#### Mini-Goal 6.4: Pattern Matching Test Suite
Verify safety, exhaustiveness warnings, and evaluation correctness.

* **Step 6.4.1**: Author test cases in `tests/unit/test_patterns.ml` for nested lists, Option/Result types, and record destructuring.
* **Step 6.4.2**: Verify that missing constructor cases produce a diagnostic listing the missing constructor.
* **Step 6.4.3**: Verify that redundant patterns produce dead-code diagnostics.
* **Step 6.4.4**: Verify that evaluating pattern matches in the interpreter produces deterministic outcomes. Run `./verify.sh`.

---

## Phase 7: Module System, Namespaces & Compilation Units

### Objective
Implement a multi-file compilation pipeline with hierarchical namespaces, explicit interface contracts (`.vesperi`), cyclic dependency detection, and deterministic linking order ([**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention) & [**Law 5**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination)).

### Governance & Escalation Triggers
* **File Extensions & Conventions**: Introducing `.vesperi` interface files and multi-file project specifications.
* **Module Grammar**: Introducing `module`, `open`, `import` keyword tokens.

### Deliverables
* [`lib/mod_ast.mli`](file:///Users/zyndrex/vesper-sandbox/lib/mod_ast.mli) / [`lib/mod_ast.ml`](file:///Users/zyndrex/vesper-sandbox/lib/mod_ast.ml)
* [`lib/namespace.mli`](file:///Users/zyndrex/vesper-sandbox/lib/namespace.mli) / [`lib/namespace.ml`](file:///Users/zyndrex/vesper-sandbox/lib/namespace.ml)
* [`lib/dep_graph.mli`](file:///Users/zyndrex/vesper-sandbox/lib/dep_graph.mli) / [`lib/dep_graph.ml`](file:///Users/zyndrex/vesper-sandbox/lib/dep_graph.ml)
* [`lib/module_check.mli`](file:///Users/zyndrex/vesper-sandbox/lib/module_check.mli) / [`lib/module_check.ml`](file:///Users/zyndrex/vesper-sandbox/lib/module_check.ml)
* [`tests/unit/test_modules.ml`](file:///Users/zyndrex/vesper-sandbox/tests/unit/test_modules.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 7.1: Module Grammar & Compilation Unit Structure
Define syntax and structures for modules, interfaces, and visibility.

* **Step 7.1.1**: Define module syntax in `lib/mod_ast.mli`: module declarations (`module M = struct ... end`), module signatures (`sig ... end`), and import statements.
* **Step 7.1.2**: Support qualified identifier path lookups (`A.B.c`) with span tracking across all segments.
* **Step 7.1.3**: Update `lib/parser.mly` with module productions.
* **Step 7.1.4**: Update `lib/printer.ml` to format modules and qualified paths.

#### Mini-Goal 7.2: Dependency Graph & Cyclic Dependency Resolution
Detect dependencies among source files and resolve valid build order.

* **Step 7.2.1**: Implement file scanner extracting imported module names without running full parsing passes.
* **Step 7.2.2**: Build directed acyclic graph (DAG) of modules in `lib/dep_graph.ml`.
* **Step 7.2.3**: Perform Tarjan's strongly connected components algorithm to detect cyclic module dependencies.
* **Step 7.2.4**: Return structured error `Error { span; message = "Cyclic dependency detected: A -> B -> A" }` on cycle detection without panics.
* **Step 7.2.5**: Produce a topologically sorted list of compilation units for sequential processing.

#### Mini-Goal 7.3: Interface Conformance & Namespace Resolution
Enforce public/private encapsulation and interface specifications.

* **Step 7.3.1**: Implement symbol resolution tables in `lib/namespace.ml` mapping qualified paths to canonical definitions.
* **Step 7.3.2**: Check implementation files against their interface files (`.vesperi`): assert that all declared types and values exist with matching types.
* **Step 7.3.3**: Hide private helper functions and internal types from foreign module consumers.
* **Step 7.3.4**: Handle `open M` shadowing rules deterministically.

#### Mini-Goal 7.4: Multi-File Integration Test Suite
Verify multi-file projects and separate compilation.

* **Step 7.4.1**: Create multi-module fixture in `tests/fixtures/modules/` with cross-module function calls and types.
* **Step 7.4.2**: Verify that cycle detection triggers structured diagnostics.
* **Step 7.4.3**: Verify that interface mismatches produce descriptive type discrepancy errors with source coordinates.
* **Step 7.4.4**: Run `./verify.sh` to ensure full compliance.

---

## Phase 8: Bytecode Virtual Machine & Bytecode Compiler

### Objective
Develop a high-performance stack-based virtual machine and bytecode compilation pipeline for Vesper. Maintain absolute behavioral equivalence and bit-for-bit determinism with the tree-walking interpreter ([**Law 5**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination)).

### Governance & Escalation Triggers
* **Bytecode Schema**: Designing the opcode set is an architectural milestone.
* **Dual-Execution Conformance**: VM and tree-walking interpreter outputs must match identically for all programs.

### Deliverables
* [`lib/opcode.mli`](file:///Users/zyndrex/vesper-sandbox/lib/opcode.mli) / [`lib/opcode.ml`](file:///Users/zyndrex/vesper-sandbox/lib/opcode.ml)
* [`lib/bytecode.mli`](file:///Users/zyndrex/vesper-sandbox/lib/bytecode.mli) / [`lib/bytecode.ml`](file:///Users/zyndrex/vesper-sandbox/lib/bytecode.ml)
* [`lib/compiler.mli`](file:///Users/zyndrex/vesper-sandbox/lib/compiler.mli) / [`lib/compiler.ml`](file:///Users/zyndrex/vesper-sandbox/lib/compiler.ml)
* [`lib/vm.mli`](file:///Users/zyndrex/vesper-sandbox/lib/vm.mli) / [`lib/vm.ml`](file:///Users/zyndrex/vesper-sandbox/lib/vm.ml)
* [`lib/disasm.mli`](file:///Users/zyndrex/vesper-sandbox/lib/disasm.mli) / [`lib/disasm.ml`](file:///Users/zyndrex/vesper-sandbox/lib/disasm.ml)
* [`tests/laws/test_vm_determinism.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_vm_determinism.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 8.1: Instruction Set Architecture (ISA) & Chunk Representation
Specify stack-based bytecode instructions, constant pools, and debug span tables.

* **Step 8.1.1**: Define opcodes in `lib/opcode.mli`:
  ```ocaml
  type t =
    | Op_Const of int
    | Op_Add | Op_Sub | Op_Mul | Op_Div
    | Op_Eq | Op_Lt
    | Op_GetLocal of int | Op_SetLocal of int
    | Op_Jump of int | Op_JumpIfFalse of int
    | Op_Call of int | Op_Return
    | Op_Halt
  ```
* **Step 8.1.2**: Define `Bytecode.chunk` containing:
  - Code buffer: opcode array
  - Constant pool: `Value.t array`
  - Line/span table: array mapping each bytecode offset to an `Ast.span` ([**Law 3**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-3-strict-source-provenance-span-retention))
* **Step 8.1.3**: Implement disassembler in `lib/disasm.ml` to inspect compiled chunks in readable textual format.

#### Mini-Goal 8.2: Core IR to Bytecode Compiler
Compile desugared Core IR into bytecode chunks with lexical scope offsets.

* **Step 8.2.1**: Implement compiler context in `lib/compiler.ml` tracking local variable scope stack and constant deduplication.
* **Step 8.2.2**: Emit opcodes for literals, resolving constant pool indexes.
* **Step 8.2.3**: Emit jumps and backpatch jump offsets for conditional expressions and loops.
* **Step 8.2.4**: Compile function closures into separate bytecode chunks or function headers with upvalue/environment capturing.
* **Step 8.2.5**: Record bytecode instruction spans in the chunk span table at every emission point.

#### Mini-Goal 8.3: Bytecode Virtual Machine Execution Loop
Build an efficient, deterministic virtual machine interpreter.

* **Step 8.3.1**: Define VM state in `lib/vm.mli`:
  ```ocaml
  type vm_state = {
    chunk : Bytecode.chunk;
    ip : int;
    stack : Value.t array;
    sp : int;
    call_stack : call_frame list;
  }
  ```
* **Step 8.3.2**: Implement VM dispatch loop using total pattern matching over opcodes.
* **Step 8.3.3**: Ensure runtime errors in the VM (stack underflow, invalid division) retrieve the corresponding `Ast.span` from the chunk span table and return structured `Diagnostic.t` without host crashes.
* **Step 8.3.4**: Integrate fuel/step limits into the VM loop to prevent unbounded execution.

#### Mini-Goal 8.4: Tree-Walking vs. VM Dual-Execution Conformance Suite
Prove exact behavioral parity between interpreter engines.

* **Step 8.4.1**: In `tests/laws/test_vm_determinism.ml`, run 500 test programs through both the Phase 4 tree-walking interpreter and the Phase 8 bytecode VM.
* **Step 8.4.2**: Compare returned `Value.t` and diagnostic errors across all runs.
* **Step 8.4.3**: Assert bit-for-bit output identity between both runtimes.
* **Step 8.4.4**: Run `./verify.sh` to confirm compilation and test success.

---

## Phase 9: Sandboxed Standard Library, Effect Isolation & Host Boundary

### Objective
Deliver a standard library (primitives for collections, strings, math, option, and result) alongside a capability-based, sandboxed effect runtime. Ensure side-effecting operations (file I/O, console output) remain fully deterministic, isolated, and mockable ([**Law 1**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-1-total-diagnosability--host-isolation-no-host-crashes) & [**Law 5**](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination)).

### Governance & Escalation Triggers
* **Effect Isolation**: Ambient I/O operations are strictly forbidden. All I/O must pass through an explicit capability boundary.
* **External Dependencies**: No third-party C bindings or external OPAM dependencies without an approved escalation.

### Deliverables
* [`lib/stdlib/core_math.mli`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_math.mli) / [`.ml`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_math.ml)
* [`lib/stdlib/core_string.mli`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_string.mli) / [`.ml`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_string.ml)
* [`lib/stdlib/core_list.mli`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_list.mli) / [`.ml`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_list.ml)
* [`lib/stdlib/core_map.mli`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_map.mli) / [`.ml`](file:///Users/zyndrex/vesper-sandbox/lib/stdlib/core_map.ml)
* [`lib/effect_runtime.mli`](file:///Users/zyndrex/vesper-sandbox/lib/effect_runtime.mli) / [`lib/effect_runtime.ml`](file:///Users/zyndrex/vesper-sandbox/lib/effect_runtime.ml)
* [`lib/prelude.mli`](file:///Users/zyndrex/vesper-sandbox/lib/prelude.mli) / [`lib/prelude.ml`](file:///Users/zyndrex/vesper-sandbox/lib/prelude.ml)
* [`tests/unit/test_stdlib.ml`](file:///Users/zyndrex/vesper-sandbox/tests/unit/test_stdlib.ml)
* [`tests/laws/test_io_isolation.ml`](file:///Users/zyndrex/vesper-sandbox/tests/laws/test_io_isolation.ml)

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 9.1: Pure Standard Library Implementation
Write pure, immutable core modules adhering to total diagnosability.

* **Step 9.1.1**: Implement `Core_math` with checked arithmetic, absolute value, powers, min/max.
* **Step 9.1.2**: Implement `Core_string` with length, slice, concat, split, trim, and formatted conversions.
* **Step 9.1.3**: Implement `Core_list` with pure functional operations: `map`, `filter`, `fold_left`, `fold_right`, `zip`, `reverse`, `length`.
* **Step 9.1.4**: Implement `Core_option` and `Core_result` monads for ergonomic, crash-free data handling.
* **Step 9.1.5**: Expose all functions through `.mli` signatures without exposing raw OCaml host exceptions.

#### Mini-Goal 9.2: Capability-Based Effect Runtime & Host Boundary
Design a sandboxed I/O system isolating the host environment from user programs.

* **Step 9.2.1**: Define capability token types in `lib/effect_runtime.mli`:
  ```ocaml
  type io_env = {
    stdin : in_channel;
    stdout : Buffer.t;
    stderr : Buffer.t;
    fs_root : string option;
  }
  ```
* **Step 9.2.2**: Implement mockable virtual file system (VFS) and memory-buffered console handles.
* **Step 9.2.3**: Prevent file reads/writes outside designated sandbox roots, returning structured errors on traversal attacks (`../`).
* **Step 9.2.4**: Implement standard library I/O primitives passing through `io_env` capabilities.

#### Mini-Goal 9.3: Standard Library Prelude Ingestion
Package standard modules into an auto-loaded prelude.

* **Step 9.3.1**: Create `lib/prelude.ml` embedding compiled AST representations of standard library functions.
* **Step 9.3.2**: Pre-populate compiler typing environment with standard library signatures during initial pass.
* **Step 9.3.3**: Pre-populate runtime environments with standard library primitive bindings.

#### Mini-Goal 9.4: Deterministic I/O Replay Suite ([Law 5 Verification](file:///Users/zyndrex/vesper-sandbox/LAWS.md#law-5-deterministic-evaluation--sound-termination))
Prove that I/O operations under identical virtual environments are completely deterministic and replayable.

* **Step 9.4.1**: In `tests/laws/test_io_isolation.ml`, run programs reading from mock inputs and emitting to virtual buffers.
* **Step 9.4.2**: Verify that repeated executions produce identical buffer bytes.
* **Step 9.4.3**: Assert that malicious file system access attempts are safely rejected with structured diagnostics.
* **Step 9.4.4**: Run `./verify.sh` to confirm all layers pass.

---

## Phase 10: Production Tooling, CLI Driver, REPL & Future Self-Hosting

### Objective
Consolidate the compiler, VM, typechecker, and standard library into a polished CLI driver (`vesper run`, `vesper check`, `vesper fmt`, `vesper repl`), create an interactive REPL, lay the foundation for a Language Server Protocol (LSP), and publish a self-hosting roadmap.

### Governance & Escalation Triggers
* **CLI Contract**: CLI arguments and command names are public APIs governed by [`PRINCIPLES.md`](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md#2-standard-escalation-triggers).
* **Final Harness Integrity**: All 4 layers of [`verify.sh`](file:///Users/zyndrex/vesper-sandbox/verify.sh) must pass without warnings or test failures.

### Deliverables
* [`bin/main.ml`](file:///Users/zyndrex/vesper-sandbox/bin/main.ml) (expanded CLI with subcommands)
* [`lib/cli.mli`](file:///Users/zyndrex/vesper-sandbox/lib/cli.mli) / [`lib/cli.ml`](file:///Users/zyndrex/vesper-sandbox/lib/cli.ml)
* [`lib/repl.mli`](file:///Users/zyndrex/vesper-sandbox/lib/repl.mli) / [`lib/repl.ml`](file:///Users/zyndrex/vesper-sandbox/lib/repl.ml)
* [`lib/lsp_server.mli`](file:///Users/zyndrex/vesper-sandbox/lib/lsp_server.mli) / [`lib/lsp_server.ml`](file:///Users/zyndrex/vesper-sandbox/lib/lsp_server.ml) (LSP protocol skeleton)
* End-to-end cram tests in `tests/cram/`
* Self-hosting specification documentation

---

### Mini-Goals & Step-by-Step Instructions

#### Mini-Goal 10.1: Production CLI Driver & Subcommands
Construct an ergonomic, Unix-standard command-line interface.

* **Step 10.1.1**: Define CLI commands in `lib/cli.mli`:
  - `vesper run <file>`: Parse, typecheck, compile to bytecode, and execute.
  - `vesper check <file>`: Parse and typecheck without executing, returning diagnostic status.
  - `vesper fmt <file>`: Losslessly parse and pretty-print source files in-place or to stdout.
  - `vesper repl`: Launch interactive session.
  - `vesper disasm <file>`: Print disassembled bytecode instructions with span mappings.
* **Step 10.1.2**: Implement argument parsing in `lib/cli.ml` handling `--help`, `--version`, and flags without uncaught exceptions on malformed arguments.
* **Step 10.1.3**: Wire commands into `bin/main.ml`.
* **Step 10.1.4**: Add Cram integration tests in `tests/cram/` validating CLI stdout/stderr outputs and exit codes.

#### Mini-Goal 10.2: Interactive Read-Eval-Print Loop (REPL)
Build a responsive, crash-resilient interactive shell.

* **Step 10.2.1**: Implement multi-line input reading loop in `lib/repl.ml`.
* **Step 10.2.2**: Maintain persistent top-level evaluation and typing environment across input lines.
* **Step 10.2.3**: Format expression evaluation results with inferred types:
  ```text
  vesper> let x = 40 + 2;
  val x : int = 42
  ```
* **Step 10.2.4**: Catch syntax or runtime errors within REPL commands, printing diagnostics to stderr and prompting for next input without terminating session.

#### Mini-Goal 10.3: Developer Tooling & LSP Server Foundation
Provide editor tooling hooks for IDE integrations.

* **Step 10.3.1**: Create `lib/lsp_server.mli` defining JSON-RPC message dispatching over standard input/output.
* **Step 10.3.2**: Implement `textDocument/didOpen` and `textDocument/didChange` handlers running `Parse_facade` and `Typecheck`, publishing diagnostics to client.
* **Step 10.3.3**: Implement `textDocument/formatting` delegating directly to `Printer.to_string` to guarantee formatting consistency.
* **Step 10.3.4**: Test LSP handlers with mocked JSON-RPC payloads in `tests/unit/test_lsp.ml`.

#### Mini-Goal 10.4: Verification, Self-Hosting Blueprint & Documentation
Finalize documentation and audit the entire codebase against the governance harness.

* **Step 10.4.1**: Document the self-hosting architecture: define minimum language subset (Core Vesper) required to write the Vesper parser and typechecker in Vesper itself.
* **Step 10.4.2**: Update [`README.md`](file:///Users/zyndrex/vesper-sandbox/README.md) with comprehensive CLI usage instructions and language guide.
* **Step 10.4.3**: Run `dune fmt` to ensure complete formatting compliance across all files.
* **Step 10.4.4**: Run full verification harness `./verify.sh` to confirm all 4 layers pass with zero warnings, zero errors, and all 5 domain laws verified.

---

## Final Verification Checklist

Every phase in this roadmap is considered complete **only** when all of the following conditions are met:

1. **Zero Warnings**: Strict compilation (`dune build @all`) succeeds with flags `(:standard -warn-error +A-4-9-27-44)`.
2. **Formatting Clean**: `dune build @fmt` reports zero differences.
3. **Domain Laws Intact**: `tests/laws/test_laws.exe` passes 100% of property and regression tests for Laws 1–5.
4. **All Unit & Cram Tests Pass**: `dune runtest` succeeds cleanly.
5. **Clean Verification Run**: `./verify.sh` executes with final output `Verification Pipeline Passed! All Laws Respected.`
