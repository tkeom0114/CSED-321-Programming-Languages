type token =
  | VAR of (string)
  | NUM of (int)
  | LPAREN
  | RPAREN
  | EOF
  | LET
  | IN
  | EQ
  | COMMA
  | FUN
  | COLON
  | BOOL
  | INT
  | UNIT
  | PLUS
  | PROD
  | ARROW
  | FST
  | SND
  | MATCH
  | INL
  | INR
  | EUNIT
  | FIX
  | REC
  | MINUS
  | DOUBLEARROW
  | WITH
  | OR
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tml.texp
