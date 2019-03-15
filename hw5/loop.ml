type action = Uml.exp -> unit
                           

(* repeats step as many times as possible *)
let rec multiStep stepf e = match stepf e with Some e' -> multiStep stepf e' | _ -> e
    

(* a stream of all steps *)
let stepStream stepf e =
  let rec steps e = 
    match stepf e with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)


let show e = print_endline (Inout.exp2string e)
                           
let eval stepf action e = action (multiStep stepf e)
                           
let wait action e =
  let _ = action e; 
          print_string "Press return:"; flush_all ();
          input_line stdin 
  in ()
       
let step stepf action e = Stream.iter action (stepStream stepf e)
                                
(* Running the actions on an interactive loop or a file *)

let loop action = Stream.iter action (Inout.read_line ())
                              
let loopFile name action = Stream.iter action (Inout.read_file name)
