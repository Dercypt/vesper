Help and version:

  $ vesper --version
  vesper 0.1.0
  $ vesper --help | head -3
  Vesper programming language
  
  Usage:

Running a program prints its final value:

  $ vesper run ok.vesper
  84

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
  error[E1008]: Type mismatch: expected 'int', but got 'bool'
    --> type_error.vesper:1:8-1:16
     |
   1 | let x = 1 + true;
     |         ^^^^^^^^
  [1]
  $ vesper run type_error.vesper 2>&1 | head -1
  error[E1008]: Type mismatch: expected 'int', but got 'bool'

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
  Constants (2):
    [0] 40
    [1] 2
  Bytecode (12 instructions):
  0000  Op_Const 0           ; 40  (ok.vesper:1:8-1:10)
  0001  Op_Const 1           ; 2  (ok.vesper:1:13-1:14)
  0002  Op_Add                (ok.vesper:1:8-1:14)

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

Legacy forms still work:

  $ vesper ok.vesper
  Successfully parsed 3 declaration(s) in ok.vesper.
  $ vesper -e 'let x = 1;'
  Successfully parsed 1 declaration(s).

REPL keeps state, survives errors, and supports multi-line input:

  $ printf 'let x = 40 + 2;\nlet g y =\n  y + x;\ng 1;\nx / 0;\nlet = ;\nx;\n:quit\n' | vesper repl
  vesper> val x : int = 42
  vesper>    ...> val g : int -> int = <fun y>
  vesper> - : int = 43
  vesper> error[E1008]: Division by zero
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
