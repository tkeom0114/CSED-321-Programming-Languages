open Lexing

let read_file fname =
    let c = open_in fname in
    let n = in_channel_length c in
    let s = really_input_string c n in 
    close_in c; s

let parse_file fname =
    let lines = read_file fname in
    let lexbuf = (from_string lines) in 
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname }; 
    try
        Parser.program Lexer.token lexbuf
    with 
    |   Parsing.Parse_error -> raise (Lexer.SyntaxError (Lexer.print_msg lexbuf "syntax error"))

let _ =
    try
        let fname = Sys.argv.(1) in
        let result = Hw4.run (parse_file fname)
        in print_endline (Imp.exp2str result)
    with
    |   Lexer.SyntaxError e -> prerr_endline e
    |   Hw4.RuntimeError e  -> prerr_endline ("Runtime error: " ^ e)
    |   Sys_error e         -> prerr_endline e
    |   Invalid_argument _  -> Printf.printf "Usage: %s <imp_file>\n" Sys.argv.(0)
