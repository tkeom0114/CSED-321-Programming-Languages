open Tml
open Hw7

type t_action = texp -> unit
type action = exp -> unit

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e = 
    match stepOpt stepf e with
    | None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in Stream.icons e (steps e)

let show e = print_endline (exp2string e)

let eval stepf action e = action (multiStep stepf (texp2exp e))

let wait action e =
  let _ = action e; 
          print_string "Press return:"; flush_all ();
          input_line stdin 
  in ()

let step stepf action e = Stream.iter action (stepStream stepf (texp2exp e))

let loop action = Stream.iter action (Inout.read_line ())

let loopFile name action = Stream.iter action (Inout.read_file name)

