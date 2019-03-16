type token =
  | INT of (int)
  | ID of (string)
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | LESS
  | AND
  | OR
  | NOT
  | LPAREN
  | RPAREN
  | SKIP
  | COLONEQ
  | SEMICOLON
  | IF
  | THEN
  | ELSE
  | FI
  | WHILE
  | DO
  | END
  | LBRACE
  | RBRACE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Imp.program
