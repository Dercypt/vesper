# Scientific Calculator

`calculator.vesper` is a scientific calculator and terminal UI implemented in pure Vesper. It compiles to bytecode and runs on the Vesper VM.

## Running

```bash
# Render ASCII dashboard
./run_calculator.sh

# Interactive prompt
./run_calculator.sh --interactive

# Direct VM execution
vesper run calculator.vesper
```

## Structure

1. **Math Engine**:
   - Arithmetic: `add`, `sub`, `mul`, `div`, `modulo` (safe zero checks).
   - `pow`: Recursive exponentiation.
   - `fact`: Recursive factorial.
   - `isqrt`: Integer square root via Newton-Raphson approximation.
   - `gcd` / `lcm`: Euclidean algorithm.
   - `fib`: Fibonacci sequence.

2. **String Serialization**:
   - `digit_to_str`: Maps `0..9` to character string.
   - `int_to_str`: Recursive integer division and modulo to convert arbitrary integers to strings without external runtime dependencies.

3. **ASCII UI**:
   - Constructs retro LCD display card, keypad matrix, and calculation tape using string concatenation.
