{
open Parser 
}
rule token = parse
  | [' ''\t''\n']                                       { token lexbuf }
  | '('                                                 { LPAREN }
  | ')'                                                 { RPAREN }
  | '='                                                 { EQ }
  | "let"                                               { LET }
  | "in"                                                { IN }
  | "fun"                                               { FUN }
  | "int"                                               { INT }
  | "bool"                                              { BOOL }
  | "true"                                              { TRUE }
  | "false"                                             { FALSE }
  | "unit"                                              { UNIT }
  | "fst"                                               { FST }
  | "snd"                                               { SND }
  | "match"                                             { MATCH }
  | "inl"                                               { INL }
  | "inr"                                               { INR }
  | "()"                                                { EUNIT }
  | "fix"                                               { FIX }
  | "rec"                                               { REC }
  | "with"                                              { WITH }
  | "if"                                                { IF }
  | "then"                                              { THEN }
  | "else"                                              { ELSE }
  | ','                                                 { COMMA }
  | ':'                                                 { COLON }
  | '+'                                                 { PLUS }
  | '-'                                                 { MINUS }
  | '*'                                                 { PROD }
  | '|'                                                 { OR }
  | "->"                                                { ARROW }
  | "=>"                                                { DOUBLEARROW }
  | ['0'-'9']+ as i                              { NUM(int_of_string i) }
  | ['A'-'Z''a'-'z'] ['A'-'Z''a'-'z''0'-'9''_''\'']* as s      { VAR(s) }
  | ';'                                                 { EOF }
  
