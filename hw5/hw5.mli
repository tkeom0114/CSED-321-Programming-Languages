exception NotImplemented 

(* one-step reduction in the call-by-value reduction strategy,
   returns NONE if impossible *)
val stepv : Uml.exp -> Uml.exp option

(* one-step reduction in the call-by-name reduction strategy, 
   returns NONE if impossible *)
val stepn : Uml.exp -> Uml.exp option
