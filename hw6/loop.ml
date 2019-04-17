
type action = Tml.exp -> unit

let show e = print_endline (Inout.exp2string e)
                           
(* Running the actions on an interactive loop or a file *)

let loop action = Stream.iter action (Inout.read_line ())
                              
let loopFile name action = Stream.iter action (Inout.read_file name)
