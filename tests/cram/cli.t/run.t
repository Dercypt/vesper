Help and version:

  $ vesper --version
  vesper 0.1.0
  $ vesper --help | head -3
  Vesper programming language
  
  Usage:

Running a program prints its final value (strings output unescaped):

  $ vesper run ok.vesper
  84
  $ printf '"hello\\nworld";\n' > str.vesper
  $ vesper run str.vesper
  hello
  world
  $ printf '1 + 2;\n' | vesper run /dev/stdin
  3

Checking:

  $ vesper check ok.vesper
  ok.vesper: ok (3 declaration(s))
  $ vesper check syntax_error.vesper
  error[E1001]: Expected expression after "+" operator. (near ";")
    --> syntax_error.vesper:1:11-1:12
     |
   1 | let x = 1 +;
     |            ^
  [1]
  $ vesper check type_error.vesper
  error[E2001]: Type mismatch: expected 'int', but got 'bool'
    --> type_error.vesper:1:8-1:16
     |
   1 | let x = 1 + true;
     |         ^^^^^^^^
  [1]
  $ vesper run type_error.vesper 2>&1 | head -1
  error[E2001]: Type mismatch: expected 'int', but got 'bool'

Formatting to stdout, and in place:

  $ vesper fmt messy.vesper
  let x = 1 + 2;
  let f a = a * x;
  f 3;
  $ printf 'let   x=1+2;\nlet f a   = a*x;\nf 3;\n' > w.vesper
  $ vesper fmt --write w.vesper
  $ cat w.vesper
  let x = 1 + 2;
  let f a = a * x;
  f 3;
  $ vesper run w.vesper
  9

Disassembly with span mappings:

  $ vesper disasm ok.vesper | head -8
  == ok.vesper ==
  Constants (1):
    [0] 42
  Bytecode (10 instructions):
  0000  Op_Const 0           ; 42  (ok.vesper:1:8-1:14)
  0001  Op_DefGlobal x        (ok.vesper:1:0-1:14)
  0002  Op_Pop                (ok.vesper:1:0-1:14)
  0003  Op_Closure 0         ; (fun a, 4 instrs)  (ok.vesper:2:15-2:20)
  $ vesper disasm type_error.vesper
  error[E2001]: Type mismatch: expected 'int', but got 'bool'
    --> type_error.vesper:1:8-1:16
     |
   1 | let x = 1 + true;
     |         ^^^^^^^^
  [1]

Malformed arguments yield usage errors, never exceptions:

  $ vesper run
  vesper: 'run' requires a file argument
  Try 'vesper --help' for usage.
  [2]
  $ vesper run a b
  vesper: 'run' takes exactly one file argument
  Try 'vesper --help' for usage.
  [2]
  $ vesper --bogus
  vesper: unknown option '--bogus'
  Try 'vesper --help' for usage.
  [2]
  $ vesper repl extra
  vesper: 'repl' takes no arguments
  Try 'vesper --help' for usage.
  [2]
  $ vesper check missing.vesper
  error[E0001]: Cannot read file "missing.vesper": missing.vesper: No such file or directory
    --> <dummy>:0:0-0:0
  [1]

Inline expressions (-e) parse and validate, while raw files require subcommands:

  $ vesper ok.vesper
  vesper: unknown command 'ok.vesper'
  Try 'vesper --help' for usage.
  [2]
  $ vesper -e ""
  $ vesper -e 'let x = 1;'
  1
  $ vesper -e '2 + 2;'
  4
  $ printf '10\n20\n' | vesper -e 'let a = read_int ""; let b = read_int ""; a + b;'
  30
  $ vesper -e '1 + true;'
  error[E2001]: Type mismatch: expected 'int', but got 'bool'
    --> <cli>:1:0-1:8
     |
   1 | 1 + true;
     | ^^^^^^^^
  [1]
  $ vesper -e '10 / 0;'
  error[E5001]: Division by zero
    --> <cli>:1:0-1:6
     |
   1 | 10 / 0;
     | ^^^^^^
  [1]

REPL keeps state, survives errors, and supports multi-line input:

  $ printf 'let x = 40 + 2;\nlet g y =\n  y + x;\ng 1;\nx / 0;\nlet = ;\nx;\n:quit\n' | vesper repl
  vesper> val x : int = 42
  vesper>    ...> val g : int -> int = <fun y>
  vesper> - : int = 43
  vesper> error[E5001]: Division by zero
    --> <repl>:1:0-1:5
     |
   1 | x / 0;
     | ^^^^^
  vesper> error[E1001]: Expected identifier or "rec" after "let" in declaration. (near "=")
    --> <repl>:1:4-1:5
     |
   1 | let = ;
     |     ^
  vesper> - : int = 42
  vesper> 
