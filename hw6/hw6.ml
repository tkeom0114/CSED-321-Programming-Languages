open Tml

exception TypeError

(***************************************************** 
 * replace unit by your own type for typing contexts *
 *****************************************************)
type context = var -> tp

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)

let createEmptyContext () = fun _ -> raise TypeError 

let updateContext cxt x a = fun y -> try(cxt y) with TypeError -> if y=x then a else raise TypeError

(* val typing : context -> Tml.exp -> Tml.tp *)
let rec typing cxt e = match e with
| Var(x) -> cxt x
| Lam(x,a,e') -> Fun(a,typing (updateContext cxt x a) e')
| App(e',e'') -> (match typing cxt e' with
                | Fun(a,b) -> if typing cxt e'' = a  then b else raise TypeError
                | _ -> raise TypeError)
| Pair(e1,e2) -> Prod(typing cxt e1, typing cxt e2)
| Fst(e') -> (match typing cxt e' with
            | Prod(a,b) -> a
            | _ -> raise TypeError)
| Snd(e') -> (match typing cxt e' with
            | Prod(a,b) -> b
             |_ -> raise TypeError)
| Eunit -> Unit
| Inl(e',a2) -> Sum(typing cxt e',a2)
| Inr(e',a1) -> Sum(a1,typing cxt e')
| Case(e',x1,e1,x2,e2) -> (match typing cxt e' with
                          | Sum(a1,a2) -> if typing (updateContext cxt x1 a1) e1 = typing (updateContext cxt x2 a2) e2 then typing (updateContext cxt x1 a1) e1
                                        else raise TypeError
                          | _ -> raise TypeError)
| Fix(x,a,e') -> if typing (updateContext cxt x a) e' = a then a else raise TypeError
| True -> Bool
| False -> Bool
| Ifthenelse(e',e1,e2) -> (match typing cxt e' with
                          | Bool -> if typing cxt e1 = typing cxt e2 then typing cxt e1 else raise TypeError
                          | _ -> raise TypeError)
| Num(n) -> Int
| Plus -> Fun(Prod(Int,Int),Int)
| Minus -> Fun(Prod(Int,Int),Int)
| Eq -> Fun(Prod(Int,Int),Bool)

let typeOf e = typing (createEmptyContext ()) e 
let typeOpt e = try Some (typeOf e) with TypeError -> None

