# Vesper CLI Reference Manual

This manual documents the production command-line interface (`vesper`) provided by the Vesper compiler toolchain.

---

## 1. Synopsis

```bash
vesper <command> [options] [arguments]
```

### Exit Codes
Vesper uses strict, standardized Unix exit codes:
* `0`: Success.
* `1`: Compilation, syntax, type, or evaluation diagnostics encountered.
* `2`: Command-line usage error (invalid flag or missing arguments).

All invocations guarantee **Law 1 (Total Diagnosability & Host Isolation)**: malformed arguments or invalid syntax never crash the process or print raw host stack traces.

---

## 2. Commands & Subcommands

### 2.1 `vesper run <file>`
Parses, typechecks, lowers to Core IR, compiles to bytecode, and executes the target Vesper script on the stack VM.
* Prints the final evaluated expression to standard output.
* If any syntax or type errors occur, prints structured diagnostics to standard error and exits with code `1`.

**Example**:
```bash
vesper run hello.vesper
```

---

### 2.2 `vesper check <file>`
Validates the syntax and static type semantics of a Vesper source file without executing it.
* Useful for fast CI validation and editor linters.
* Outputs the number of declarations checked upon success.

**Example**:
```bash
vesper check calculator.vesper
# Output: calculator.vesper: ok (37 declaration(s))
```

---

### 2.3 `vesper fmt [--write] <file>`
Formats a Vesper source file using the canonical, lossless pretty-printer.
* By default, writes the formatted source to standard output.
* When `--write` is specified, rewrites the file in place.
* Formatting satisfies **Law 2**: `parse ∘ print ∘ parse = parse`.

**Examples**:
```bash
# Pretty-print to terminal:
vesper fmt script.vesper

# Format file in place:
vesper fmt --write script.vesper
```

---

### 2.4 `vesper repl`
Launches the interactive Read-Eval-Print Loop.
* Maintains a persistent typing and evaluation environment across expressions.
* Handles syntax and runtime errors gracefully without terminating the session.
* Commands:
  * Type any declaration (e.g. `let x = 40 + 2;`) or expression (e.g. `x + 1;`).
  * Type `:quit` or `:q` to exit.

**Example**:
```text
$ vesper repl
vesper> let x = 40 + 2;
val x : int = 42
vesper> x + 1;
- : int = 43
vesper> :quit
```

---

### 2.5 `vesper disasm <file>`
Compiles the specified source file into Vesper bytecode and disassembles the instruction stream, displaying:
* Total constant pool entries.
* Numerical opcode offsets and mnemonics (`CONST`, `ADD`, `LOAD`, `STORE`, `JUMP`, etc.).
* Source span mappings for each instruction.

**Example**:
```bash
vesper disasm calculator.vesper
```

---

### 2.6 `vesper lsp`
Runs the Language Server Protocol (LSP) server communicating over JSON-RPC on `stdin` and `stdout`.
* Supports LSP capabilities:
  * `textDocument/didOpen`: Publishes syntax and type diagnostics.
  * `textDocument/didChange`: Incremental diagnostic updates.
  * `textDocument/didClose`: Cleans up buffers.
  * `textDocument/formatting`: In-editor formatting using `Printer.to_string`.

**Example**:
```bash
vesper lsp
```

---

### 2.7 Informational Flags & Legacy Modes

* `vesper --help` / `vesper -h`: Displays command options and usage information.
* `vesper --version` / `vesper -v`: Displays the compiler version.
* `vesper <file>`: Legacy alias for parsing and checking a file.
* `vesper -e "<expr>"`: Evaluates a short inline expression from the command line.
