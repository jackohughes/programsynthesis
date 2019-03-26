type token =
  | VAR of (string)
  | NUM of (int)
  | BOOL of (bool)
  | LET
  | EQ
  | IN
  | IF
  | THEN
  | ELSE
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | GT
  | GE
  | LS
  | LE
  | NOT
  | TREE
  | CHILDREN
  | VALUE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COMMA
  | MAP
  | FILT
  | FOLDL
  | FOLDR
  | MAPT
  | FOLDT
  | FST
  | SND
  | FUN
  | ARROW
  | EQI
  | EQB
  | NEQI
  | NEQB
  | HEAD
  | TAIL
  | INDEX
  | EOF

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
val examples :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Ast.expr * Ast.expr) list
val ex :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Ast.expr * Ast.expr)
