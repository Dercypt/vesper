# Vesper Governance Harness

The Vesper Governance Harness is an automated verification pipeline designed to enforce the domain invariants defined in [`LAWS.md`](file:///Users/zyndrex/vesper-sandbox/LAWS.md) and the engineering principles defined in [`PRINCIPLES.md`](file:///Users/zyndrex/vesper-sandbox/PRINCIPLES.md).

---

## Harness Structure

The harness operates through four defensive layers:

```
+-------------------------------------------------------------+
| Layer 1: Code Style & Formatting (dune build @fmt)          |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
| Layer 2: Strict Compilation (dune build @all -warn-error)   |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
| Layer 3: Standard Unit & Cram Tests (dune runtest)          |
+-------------------------------------------------------------+
                              |
                              v
+-------------------------------------------------------------+
| Layer 4: Invariant & Law Suite (tests/laws/test_laws.exe)   |
+-------------------------------------------------------------+
```

---

## Verification Pipeline: `verify.sh`

The canonical entry point for checking the codebase is `./verify.sh`. This script is executed locally prior to submitting changes and in CI/CD pipelines.

### Pipeline Stages

1. **Toolchain Environment Check**:
   Ensures `opam` and `dune` are present in the shell environment. If `opam` is installed but its environment has not been evaluated in the current shell, `verify.sh` automatically evaluates `eval $(opam env)` if possible.
2. **Formatting Enforcement**:
   Runs `dune build @fmt`. Fails if any file does not conform to the `.ocamlformat` rules (run `dune fmt` to format automatically).
3. **Strict Compilation**:
   Runs `dune build @all`. Enforces `(:standard -warn-error +A-4-9-27-44)` across all libraries, executables, and test suites.
4. **Test Suite & Invariant Execution**:
   Runs `dune runtest`, which discovers and executes:
   * Standard unit tests and integration tests.
   * Dedicated invariant tests in `tests/laws/` (validating Laws 1–5).

---

## Running the Verification Pipeline

To execute the full verification harness:

```bash
./verify.sh
```

To automatically format the codebase to satisfy Layer 1:

```bash
dune fmt
```

To run only the invariant law test suite:

```bash
dune exec tests/laws/test_laws.exe
```
