# CLI Reference

## Synopsis

```bash
vesper <command> [options] [arguments]
```

## Commands

| Command | Description |
| :--- | :--- |
| `vesper run <file>` | Parse, typecheck, compile, and execute on bytecode VM. |
| `vesper check <file>` | Parse and typecheck without execution. |
| `vesper fmt <file>` | Print canonically formatted source to stdout. |
| `vesper fmt --write <file>` | Format source file in place. |
| `vesper repl` | Start interactive REPL (`:quit` to exit). |
| `vesper disasm <file>` | Disassemble bytecode with source spans. |
| `vesper lsp` | Run LSP server over stdin/stdout. |
| `vesper -e <source>` | Parse, typecheck, compile, and execute inline source on bytecode VM. |
| `vesper --help` | Show usage text. |
| `vesper --version` | Print compiler version. |

## Exit Codes

* `0`: Success.
* `1`: Diagnostic error (syntax, type, or runtime).
* `2`: Usage error (invalid flag or missing arguments).
