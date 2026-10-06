#!/usr/bin/env bash
# =========================================================================
# Vesper Calculator UI Launcher & Interactive Terminal Display
# =========================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN="$SCRIPT_DIR/_build/default/bin/main.exe"

# Ensure Vesper toolchain is built
if [ ! -f "$BIN" ]; then
    echo "Building Vesper..."
    if command -v opam &>/dev/null; then
        eval "$(opam env)"
    fi
    dune build bin/main.exe
fi

render_vesper_output() {
    python3 -c '
import sys, ast
raw = sys.stdin.read().strip()
if raw.startswith("\"") and raw.endswith("\""):
    try:
        print(ast.literal_eval(raw))
    except Exception:
        print(raw[1:-1].encode().decode("unicode-escape"))
else:
    print(raw)
'
}

show_static_calc() {
    "$BIN" run "$SCRIPT_DIR/calculator.vesper" | render_vesper_output
}

interactive_mode() {
    clear || true
    echo "=========================================================="
    echo "       Starting Interactive Vesper Calculator Session     "
    echo "=========================================================="
    echo "Commands:"
    echo "  - Type any arithmetic expression (e.g. 15 * 8, fact 5, pow 2 10, gcd 48 18)"
    echo "  - Type 'demo' to show the full showcase UI dashboard"
    echo "  - Type 'quit' or 'exit' to exit"
    echo "=========================================================="
    echo ""

    local last_expr="125 + 75"
    local last_ans="200"

    while true; do
        printf "\033[1;36mvesper-calc > \033[0m"
        read -r line || break
        case "$line" in
            "quit"|"exit"|"q")
                echo "Goodbye!"
                break
                ;;
            "demo"|"show"|"ui")
                show_static_calc
                echo ""
                ;;
            "")
                continue
                ;;
            *)
                # Create a temporary Vesper evaluation script with the prelude & calculator engine
                TMP_FILE=$(mktemp /tmp/vesper_calc_XXXXXX.vesper)
                cat << 'EOF' > "$TMP_FILE"
let add a b = a + b;
let sub a b = a - b;
let mul a b = a * b;
let div a b = if b == 0 then 0 else a / b;
let modulo a b = if b == 0 then 0 else a % b;

let rec pow b e =
  if e <= 0 then 1
  else b * pow b (e - 1);

let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1);

let rec isqrt_guess n g =
  if g * g <= n && (g + 1) * (g + 1) > n then g
  else if g == 0 then 0
  else isqrt_guess n ((g + n / g) / 2);

let isqrt n =
  if n <= 0 then 0
  else isqrt_guess n (n / 2 + 1);

let rec gcd a b =
  if b == 0 then a
  else gcd b (a % b);

let lcm a b =
  if a == 0 || b == 0 then 0
  else (a * b) / (gcd a b);

let rec fib n =
  if n <= 0 then 0
  else if n == 1 then 1
  else fib (n - 1) + fib (n - 2);

let digit_to_str d =
  if d == 0 then "0"
  else if d == 1 then "1"
  else if d == 2 then "2"
  else if d == 3 then "3"
  else if d == 4 then "4"
  else if d == 5 then "5"
  else if d == 6 then "6"
  else if d == 7 then "7"
  else if d == 8 then "8"
  else if d == 9 then "9"
  else "?";

let rec int_to_str_pos n =
  if n < 10 then digit_to_str n
  else "" + (int_to_str_pos (n / 10)) + (digit_to_str (n % 10));

let int_to_str n =
  if n < 0 then "-" + (int_to_str_pos (-n))
  else int_to_str_pos n;

EOF
                echo "let user_val = $line;" >> "$TMP_FILE"
                echo "user_val;" >> "$TMP_FILE"

                # Run in Vesper
                if RES=$("$BIN" run "$TMP_FILE" 2>&1); then
                    last_expr="$line"
                    last_ans="$RES"
                    
                    # Print LCD display card
                    echo ""
                    echo "+---------------------------------------------------+"
                    echo "|  [ LCD DISPLAY ]                     STATUS: OK   |"
                    echo "|  +---------------------------------------------+  |"
                    printf "|  |  EXPR: %-36s |  |\n" "$last_expr"
                    printf "|  |  ANS : = %-34s |  |\n" "$last_ans"
                    echo "|  +---------------------------------------------+  |"
                    echo "+---------------------------------------------------+"
                    echo ""
                else
                    echo -e "\033[1;31mCalculation error:\033[0m"
                    echo "$RES"
                    echo ""
                fi
                rm -f "$TMP_FILE"
                ;;
        esac
    done
}

if [ "${1:-}" = "--interactive" ] || [ "${1:-}" = "-i" ]; then
    interactive_mode
else
    show_static_calc
    echo ""
    echo "Tip: Run './run_calculator.sh --interactive' for live interactive calculation mode."
fi
