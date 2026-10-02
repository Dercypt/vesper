%{
open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token <string> IDENT

%token LET
%token REC
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token MATCH
%token WITH

%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT

%token EQEQ
%token NOTEQ
%token LT
%token LTE
%token GT
%token GTE

%token AND
%token OR
%token NOT

%token EQUAL
%token ARROW
%token COLON
%token SEMICOLON
%token COMMA

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token BAR

%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%left OR
%left AND
%left EQEQ NOTEQ
%left LT LTE GT GTE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS NOT

%start <Ast.program> program
%start <Ast.expr> expr_entry

%%

program:
  | decls = decl_seq EOF
    { { span = Ast.span_of_positions $loc; decls } }
;

expr_entry:
  | e = expr EOF
    { e }
;

decl_seq:
  | /* empty */
    { [] }
  | d = decl SEMICOLON rest = decl_seq
    { d :: rest }
  | d = decl
    { [d] }
;

decl:
  | LET r = is_rec name = IDENT args = var_list EQUAL value = expr
    {
      {
        span = Ast.span_of_positions $loc;
        decl_desc = LetDecl { name; is_rec = r; args; value };
      }
    }
  | e = expr
    {
      {
        span = Ast.span_of_positions $loc;
        decl_desc = ExprDecl e;
      }
    }
;

is_rec:
  | REC
    { true }
  | /* empty */
    { false }
;

var_list:
  | /* empty */
    { [] }
  | id = IDENT rest = var_list
    { id :: rest }
;

var_nonempty_list:
  | id = IDENT
    { [id] }
  | id = IDENT rest = var_nonempty_list
    { id :: rest }
;

expr:
  | LET r = is_rec name = IDENT args = var_list EQUAL value = expr IN body = expr
    {
      {
        span = Ast.span_of_positions $loc;
        desc = Let { name; is_rec = r; args; value; body };
      }
    }
  | IF cond = expr THEN then_branch = expr ELSE else_branch = expr
    {
      {
        span = Ast.span_of_positions $loc;
        desc = If { cond; then_branch; else_branch };
      }
    }
  | FUN params = var_nonempty_list ARROW body = expr
    {
      {
        span = Ast.span_of_positions $loc;
        desc = Fun { params; body };
      }
    }
  | lhs = expr PLUS rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Add; lhs; rhs } } }
  | lhs = expr MINUS rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Sub; lhs; rhs } } }
  | lhs = expr STAR rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Mul; lhs; rhs } } }
  | lhs = expr SLASH rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Div; lhs; rhs } } }
  | lhs = expr PERCENT rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Mod; lhs; rhs } } }
  | lhs = expr EQEQ rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Eq; lhs; rhs } } }
  | lhs = expr NOTEQ rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Neq; lhs; rhs } } }
  | lhs = expr LT rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Lt; lhs; rhs } } }
  | lhs = expr LTE rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Le; lhs; rhs } } }
  | lhs = expr GT rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Gt; lhs; rhs } } }
  | lhs = expr GTE rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Ge; lhs; rhs } } }
  | lhs = expr AND rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = And; lhs; rhs } } }
  | lhs = expr OR rhs = expr
    { { span = Ast.span_of_positions $loc; desc = Binary { op = Or; lhs; rhs } } }
  | MINUS arg = expr %prec UMINUS
    { { span = Ast.span_of_positions $loc; desc = Unary { op = Neg; arg } } }
  | NOT arg = expr %prec NOT
    { { span = Ast.span_of_positions $loc; desc = Unary { op = Not; arg } } }
  | e = app_expr
    { e }
;

app_expr:
  | fn = app_expr arg = atomic_expr
    { { span = Ast.span_of_positions $loc; desc = App { fn; arg } } }
  | e = atomic_expr
    { e }
;

atomic_expr:
  | n = INT
    { { span = Ast.span_of_positions $loc; desc = Lit (Int n) } }
  | b = BOOL
    { { span = Ast.span_of_positions $loc; desc = Lit (Bool b) } }
  | s = STRING
    { { span = Ast.span_of_positions $loc; desc = Lit (String s) } }
  | id = IDENT
    { { span = Ast.span_of_positions $loc; desc = Var id } }
  | LPAREN e = expr RPAREN
    { { span = Ast.span_of_positions $loc; desc = e.desc } }
;
