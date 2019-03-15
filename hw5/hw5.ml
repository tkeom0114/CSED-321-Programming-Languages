exception NotImplemented 

let freshVarCounter = ref 0
                          
(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s.  *)
let getFreshVariable s = 
  let _ = freshVarCounter := !freshVarCounter + 1 in
  s ^ "__" ^ (string_of_int (!freshVarCounter))
               

(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv e = raise NotImplemented         


(*
 * implement a single step with reduction using the call-by-name strategy.
 *)
let rec stepn e = raise NotImplemented         
