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


let run_program (_,exp) flag =
    let rec steps config =
        if flag then print_endline ((Hw8.config2str config) ^ "\n");
        match Hw8.final config with
        | Some v -> v
        | _      -> steps (Hw8.step config) in 
    steps (Hw8.initial exp)


let _ =
    try
        let fname = Sys.argv.(1) in 
        let program = parse_file fname in 
        let flag = Array.length Sys.argv > 2 && Sys.argv.(2) = "-debug" in
        let result = run_program (Validate.vprogram program) flag in
        print_endline (Print.exp2str (Hw8.value2exp result))
    with
    |   Lexer.SyntaxError err -> prerr_endline err
    |   Sys_error err         -> prerr_endline err
    |   Invalid_argument _  -> Printf.printf "Usage: %s <imp_file>\n" Sys.argv.(0)

