{
    open Lexing
    open Parser
    open Printf

    exception Eof
    exception SyntaxError of string

    let comment_depth = ref 0

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
let name    = ['a'-'z']['a'-'z''A'-'Z''0'-'9''_''\'']*
let cname   = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_''\'']*
let number  = ['0'-'9']+

rule token = parse
  | white           { token lexbuf }
  | newline         { next_line lexbuf; token lexbuf }
  | "(*"            { comment_depth := 1; ignore (comment lexbuf); token lexbuf }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "int"           { INT }
  | "bool"          { BOOL } 
  | "unit"          { UNIT }
  | "true" as b     { TRUE(bool_of_string b) }
  | "false" as b    { FALSE(bool_of_string b) }
  | "()"            { EUNIT }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { PROD }
  | '='             { EQ }
  | '<'             { LESS }
  | "->"            { ARROW }
  | ':'             { COLON }
  | '_'             { UNDERSCORE }
  | "match"         { MATCH }
  | "with"          { WITH }
  | "fun"           { FUN }
  | "let"           { LET }
  | "in"            { IN }
  | "of"            { OF }
  | "rec"           { REC }
  | "ref"           { REF }
  | "!"             { BANG }
  | ":="            { COLONEQUAL }
  | "type"          { TYPE }
  | ','             { COMMA }  
  | '|'             { BAR }
  | ";;"            { ENDDEC }
  | number as i     { NUM(int_of_string i) }
  | cname as s      { CID(s) }
  | name as s       { VID(s) }
  | eof             { EOF }
  | _ as c          { raise (SyntaxError(print_msg lexbuf (sprintf "unexpected char '%c'" c))) }

and comment = parse
  | "(*"    { comment_depth := !comment_depth + 1; comment lexbuf}
  | "*)"    { comment_depth := !comment_depth - 1; if !comment_depth > 0 then comment lexbuf }
  | eof     { raise Eof }
  | _       { comment lexbuf }

