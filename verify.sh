#!/usr/bin/env bash
set -euo pipefail

# ANSI Color codes
BOLD="\033[1m"
GREEN="\033[0;32m"
YELLOW="\033[0;33m"
RED="\033[0;31m"
BLUE="\033[0;34m"
RESET="\033[0m"

log_info() {
    echo -e "${BLUE}[INFO]${RESET} $1"
}

log_pass() {
    echo -e "${GREEN}[PASS]${RESET} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${RESET} $1"
}

log_fail() {
    echo -e "${RED}[FAIL]${RESET} $1"
}

echo -e "${BOLD}========================================${RESET}"
echo -e "${BOLD}       Vesper Verification Harness       ${RESET}"
echo -e "${BOLD}========================================${RESET}"

# 1. Environment & Toolchain Discovery
if ! command -v dune &>/dev/null; then
    OPAM_BIN=""
    if command -v opam &>/dev/null; then
        OPAM_BIN="opam"
    elif [ -x "/opt/homebrew/bin/opam" ]; then
        OPAM_BIN="/opt/homebrew/bin/opam"
    elif [ -x "/usr/local/bin/opam" ]; then
        OPAM_BIN="/usr/local/bin/opam"
    elif [ -x "$HOME/.opam/opam" ]; then
        OPAM_BIN="$HOME/.opam/opam"
    fi

    if [ -n "$OPAM_BIN" ]; then
        log_info "dune not found in PATH, attempting to activate opam environment..."
        eval "$("$OPAM_BIN" env 2>/dev/null || true)"
    fi
fi

if ! command -v dune &>/dev/null; then
    log_fail "Dune build tool not found in PATH."
    echo ""
    echo "To setup the Vesper toolchain, run:"
    echo "  brew install opam"
    echo "  opam init -y --bare"
    echo "  opam switch create vesper 5.2.0"
    echo "  eval \$(opam env)"
    echo "  opam install -y dune menhir ocamlformat alcotest"
    echo ""
    exit 1
fi

log_pass "Toolchain discovered: $(ocamlc --version 2>/dev/null || echo 'ocaml') / $(dune --version)"

# 2. Layer 1: Code Formatting
log_info "Layer 1: Checking code formatting (dune build @fmt)..."
if dune build @fmt; then
    log_pass "Formatting conforms to .ocamlformat"
else
    log_fail "Formatting violations found! Run 'dune fmt' to automatically fix them."
    exit 1
fi

# 3. Layer 2: Strict Compilation
log_info "Layer 2: Compiling codebase with strict warnings-as-errors (dune build @all)..."
if dune build @all; then
    log_pass "Strict compilation succeeded with zero warnings."
else
    log_fail "Compilation failed or warnings encountered."
    exit 1
fi

# 4. Layer 3 & 4: Unit Tests & Invariant Laws
log_info "Layer 3 & 4: Executing test suite and domain invariants (dune runtest)..."
if dune runtest; then
    log_pass "All tests and domain invariant laws verified successfully."
else
    log_fail "Test suite or invariant law check failed."
    exit 1
fi

echo -e "${BOLD}========================================${RESET}"
echo -e "${GREEN}${BOLD} Verification Pipeline Passed! All Laws Respected. ${RESET}"
echo -e "${BOLD}========================================${RESET}"
