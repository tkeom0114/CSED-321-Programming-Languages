
open Imp

exception NotImplemented

exception RuntimeError of string

type state = (id * exp) list    (* dummy type, to be chosen by students *)

(* emptystate : state *)
let emptystate = []

let to_num e = match e with
| Num(i) -> i
| _ -> raise (RuntimeError ("expected integer but "^(exp2str e)))

let to_bool e =  match e with
| Bool(b) -> b
| _ -> raise (RuntimeError ("expected boolean but "^(exp2str e)))

let div n m = if m != 0 then n / m else raise (RuntimeError ("division by zero"))


let rec eval s e  = match e with 
| Num(i)       -> Num(i)
| Bool(b)      -> Bool(b)
| Var(n)       -> if List.exists (fun a -> (fst a = n)) s then snd (List.find (fun a -> fst a = n) s) 
                  else raise (RuntimeError ("unbound identifier "^n)) 
| Add(e1, e2)  -> Num(to_num (eval s e1) + to_num (eval s e2))
| Sub(e1, e2)  -> Num(to_num (eval s e1) - to_num (eval s e2))
| Mul(e1, e2)  -> Num(to_num (eval s e1) * to_num (eval s e2))
| Div(e1, e2)  -> Num(div (to_num (eval s e1))  (to_num (eval s e2)))
| Eq(e1, e2)   -> Bool(to_num (eval s e1) = to_num (eval s e2))
| Less(e1, e2) -> Bool(to_num (eval s e1) < to_num (eval s e2))
| And(e1, e2)  -> Bool(to_bool (eval s e1) && to_bool (eval s e2))
| Or(e1, e2)   -> Bool(to_bool (eval s e1) || to_bool (eval s e2))
| Not(e1)      -> Bool(not (to_bool (eval s e1)))
(* exec : state -> stmt -> state *)
let rec exec s stm = match stm with
| Skip        -> s
| Assign(n,e) -> if List.exists (fun a -> (fst a = n)) s then (List.map (fun a-> if (fst a = n) then (n,eval s e) else a) s) 
                else (n,eval s e)::s
| Seq(stm1,stm2) -> exec (exec s stm1) stm2
| If(e,stm1,stm2) -> if to_bool (eval s e) then exec s stm1 else exec s stm2
| If2(e,stm')   -> if to_bool (eval s e) then exec s stm' else s
| While(e,stm') -> if to_bool (eval s e) then exec (exec s stm') stm else s 

(* run : program -> exp *)
let run (s,e) = eval (exec emptystate s) e

