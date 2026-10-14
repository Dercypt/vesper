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

show_static_calc() {
    "$BIN" run "$SCRIPT_DIR/calculator.vesper"
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
                # Create a temporary Vesper evaluation script utilizing standard prelude
                TMP_FILE=$(mktemp /tmp/vesper_calc_XXXXXX.vesper)
                echo "let user_val = $line;" > "$TMP_FILE"
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
