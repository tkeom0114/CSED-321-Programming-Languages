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
        let program = parse_file fname in 
        let vprogram =  Validate.vprogram program in

        let _ = print_endline "***** Input program *****" in
        let _ = print_endline (Print.program2str vprogram) in

        let tprogram = Hw9.tprogram vprogram in
        let _ = print_endline "***** Typed program *****" in
        print_endline (Typedprint.program2str tprogram)
    with
    |   Lexer.SyntaxError err -> prerr_endline err
    |   Sys_error err         -> prerr_endline err
    |   Invalid_argument _  -> Printf.printf "Usage: %s <imp_file>\n" Sys.argv.(0)

