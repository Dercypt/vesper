type t =
  | E0001_io_error
  | E0002_internal_error
  | E1001_unexpected_token
  | E1002_unterminated_string
  | E1003_unclosed_comment
  | E1004_invalid_escape
  | E1005_unexpected_char
  | E1006_integer_overflow
  | E1007_invalid_span
  | E1008_syntax_error
  | E2001_type_error
  | E3001_non_exhaustive_match
  | E3002_redundant_pattern
  | E3003_pattern_type_error
  | E4001_cyclic_dependency
  | E4002_unbound_module
  | E4003_interface_mismatch
  | E4004_private_member_access

let to_code_string = function
  | E0001_io_error -> "E0001"
  | E0002_internal_error -> "E0002"
  | E1001_unexpected_token -> "E1001"
  | E1002_unterminated_string -> "E1002"
  | E1003_unclosed_comment -> "E1003"
  | E1004_invalid_escape -> "E1004"
  | E1005_unexpected_char -> "E1005"
  | E1006_integer_overflow -> "E1006"
  | E1007_invalid_span -> "E1007"
  | E1008_syntax_error -> "E1008"
  | E2001_type_error -> "E2001"
  | E3001_non_exhaustive_match -> "E3001"
  | E3002_redundant_pattern -> "E3002"
  | E3003_pattern_type_error -> "E3003"
  | E4001_cyclic_dependency -> "E4001"
  | E4002_unbound_module -> "E4002"
  | E4003_interface_mismatch -> "E4003"
  | E4004_private_member_access -> "E4004"

let to_description = function
  | E0001_io_error -> "I/O Error"
  | E0002_internal_error -> "Internal Compiler Error"
  | E1001_unexpected_token -> "Unexpected Token"
  | E1002_unterminated_string -> "Unterminated String Literal"
  | E1003_unclosed_comment -> "Unclosed Block Comment"
  | E1004_invalid_escape -> "Invalid Character Escape"
  | E1005_unexpected_char -> "Unexpected Character"
  | E1006_integer_overflow -> "Integer Literal Overflow"
  | E1007_invalid_span -> "Invalid Source Span"
  | E1008_syntax_error -> "Syntax Error"
  | E2001_type_error -> "Type Error"
  | E3001_non_exhaustive_match -> "Non-Exhaustive Pattern Match"
  | E3002_redundant_pattern -> "Redundant Pattern Clause"
  | E3003_pattern_type_error -> "Pattern Type Mismatch"
  | E4001_cyclic_dependency -> "Cyclic Module Dependency"
  | E4002_unbound_module -> "Unbound Module Identifier"
  | E4003_interface_mismatch -> "Interface Conformance Mismatch"
  | E4004_private_member_access -> "Private Member Access Violation"

let pp fmt code = Format.pp_print_string fmt (to_code_string code)
let to_string code = to_code_string code
let equal (a : t) (b : t) = a = b
