{
    open Lexing
    open Parser
    open Printf

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p
        in lexbuf.lex_curr_p <- 
            { pos with pos_bol = lexbuf.lex_curr_pos;
                       pos_lnum = pos.pos_lnum + 1 
            }

    let print_msg lexbuf msg =
        let p = lexbuf.lex_curr_p in 
        sprintf "%s:%d:%d: %s" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol + 1) msg
}
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let iden    = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let integer = '-'?['0'-'9']+
let comment = "//"[^'\n']*'\n'

rule token = parse
  | white           { token lexbuf }
  | newline         { next_line lexbuf; token lexbuf }
  | comment         { token lexbuf }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { STAR }
  | '/'             { SLASH }
  | '='             { EQUAL }
  | '<'             { LESS }
  | "&&"            { AND }
  | "||"            { OR }
  | '!'             { NOT }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "skip"          { SKIP }
  | ":="            { COLONEQ }
  | ';'             { SEMICOLON }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "fi"            { FI }
  | "while"         { WHILE }
  | "do"            { DO }
  | "end"           { END }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | integer as i    { INT(int_of_string i) }
  | iden as n       { ID(n) }
  | eof             { EOF }
  | _ as c          { raise (SyntaxError(print_msg lexbuf (sprintf "unexpected char '%c'" c))) }

