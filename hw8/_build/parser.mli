type token =
  | VID of (string)
  | CID of (string)
  | NUM of (int)
  | TRUE of (bool)
  | FALSE of (bool)
  | LPAREN
  | RPAREN
  | INT
  | BOOL
  | UNIT
  | EUNIT
  | PLUS
  | MINUS
  | PROD
  | EQ
  | LESS
  | ARROW
  | COLON
  | UNDERSCORE
  | MATCH
  | WITH
  | FUN
  | LET
  | IN
  | OF
  | REC
  | REF
  | BANG
  | COLONEQUAL
  | TYPE
  | COMMA
  | BAR
  | ENDDEC
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tml.program
