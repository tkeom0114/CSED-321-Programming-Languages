(* translate TML expressions into expressions with de Bruijn's index *)
val texp2exp : Tml.texp -> Tml.exp

(* one-step reduction using eager evaluation; raises Stuck if impossible *)
val stepv : Tml.exp -> Tml.exp 

(* one-step reduction using lazy evaluation; raises Stuck if impossible *)
val stepn : Tml.exp -> Tml.exp 
