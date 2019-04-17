open Uml

exception NotImplemented 

let freshVarCounter = ref 0
                          
(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s.  *)
let getFreshVariable s = 
  let _ = freshVarCounter := !freshVarCounter + 1 in
  s ^ "__" ^ (string_of_int (!freshVarCounter))


let convert (a: 'a option) : 'a  = match a with
| Some a' -> a'
| None -> raise NotImplemented

let rec exist x l = match l with
  | [] -> false
  | a::l' -> (a=x) || exist x l'

let rec union s t = match t with
| [] -> s
| a::t' -> if (exist a s) then union s t' else union (a::s) t'

let rec freeVar e = match e with
| Var(x) -> [x]
| Lam(x,e') -> List.filter (fun y -> (x != y)) (freeVar e')
| App(e1,e2) -> union (freeVar e1) (freeVar e2)

let rec swap x y e = match e with
| Var(z) -> if z = x then Var(y) else if z = y then Var(x) else e
| Lam(z,e') -> if z = x then Lam(y,swap x y e') else if z = y then Lam(x,swap x y e') else Lam(z,swap x y e')
| App(e1,e2) -> App(swap x y e1,swap x y e2)

(*compute [e'/x]e *)       
let rec substitution e' x e = match e with
| Var(y) -> if x = y then e' else e
| App(e1,e2) -> App(substitution e' x e1,substitution e' x e2)
| Lam(y,e1) -> if x = y then e
              else if not (exist x (freeVar e)) then Lam(y,substitution e' x e1)
              else if not (exist y (freeVar e')) then Lam(y,substitution e' x e1)
              else let z = getFreshVariable y in Lam(z,substitution e' x (swap y z e1))


(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv e  = match e with
(*| App(Lam(x,e'),Var(v)) -> Some(substitution (Var(v)) x e')*)
| App(Lam(x,e1),Lam(y,e2)) -> Some(substitution (Lam(y,e2)) x e1)
| App(Lam(x,e'),e2) -> (try Some(App(Lam(x,e'),convert (stepv e2))) with NotImplemented -> None)
| App(e1,e2) -> (try Some(App(convert (stepv e1),e2)) with NotImplemented -> None )
| _ -> None


(*
 * implement a single step with reduction using the call-by-name strategy.
 *)
let rec stepn e = match e with
| App(Lam(x,e'),e2) -> Some(substitution e2 x e')
| App(e1,e2) ->(try Some(App(convert (stepn e1),e2)) with NotImplemented -> None) 
| _ -> None

