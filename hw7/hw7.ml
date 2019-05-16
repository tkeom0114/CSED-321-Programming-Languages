open Tml

type context = var list

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)
let rec union s t = match t with
| [] -> s
| a::t' -> if (List.mem a s) then union s t' else union (s@[a]) t'
let unionlist l = 
  let rec unionlist' a l = match l with
  | [] -> a
  | x::l' -> unionlist' (union a x) l'
  in unionlist' [] l
let delete x l = List.filter (fun y -> (x <> y)) l
let rec freeVar te = match te with
| Tvar(x) -> [x]
| Tlam(x,t,te') -> delete x (freeVar te')
| Tapp(te1,te2) -> union (freeVar te1) (freeVar te2)
| Tpair(te1,te2) -> union (freeVar te1) (freeVar te2)
| Tfst(te') -> freeVar te'
| Tsnd(te') -> freeVar te'
| Teunit -> []                     
| Tinl(te',t) -> freeVar te'          
| Tinr(te',t) -> freeVar te'              
| Tcase(te',x1,te1,x2,te2) ->  unionlist ((freeVar te')::(delete x1 (freeVar te1))::[delete x2 (freeVar te2)])
| Tfix(x,t,te') -> delete x (freeVar te')         
| Ttrue -> []
| Tfalse -> []
| Tifthenelse(te',te1,te2) ->  unionlist ((freeVar te')::(freeVar te1)::[freeVar te2])
| Tnum(i) -> []
| Tarith(op) -> []


let inde l x = 
  let rec inde' l x c = match l with
  | [] -> raise Stuck
  | a::l' -> if x = a then c else inde' l' x c+1
  in inde' l x 0

let rec typing cxt te = match te with
| Tvar(x) -> Ind(inde cxt x)
| Tlam(x,t,te') -> Lam(typing (x::cxt) te')
| Tapp(te1,te2) -> App((typing cxt te1),(typing cxt te2))    
| Tpair(te1,te2) -> Pair(typing cxt te1,typing cxt te2)           
| Tfst(te') -> Fst(typing cxt te')               
| Tsnd(te') -> Snd(typing cxt te')                          
| Teunit -> Eunit                       
| Tinl(te',t) ->  Inl(typing cxt te')                   
| Tinr(te',t) ->  Inr(typing cxt te')                    
| Tcase(te',x1,te1,x2,te2) -> Case(typing cxt te', typing (x1::cxt) te1, typing (x2::cxt) te2)   
| Tfix(x,t,te') -> Fix(typing (x::cxt) te')                
| Ttrue -> True                               
| Tfalse -> False                                
| Tifthenelse(te',te1,te2) ->  Ifthenelse(typing cxt te',typing cxt te1,typing cxt te2)       
| Tnum(i) -> Num(i)                            
| Tarith(op) -> Arith(op)                         


(* texp2exp : Tml.texp -> Tml.exp *)
let texp2exp te = typing (freeVar te) te

let rec tau n i e = match e with
| Ind(m) -> if m < i then Ind(m) else Ind(m+n)
| Lam(e') -> Lam(tau n (i+1) e')
| App(e1,e2) -> App(tau n i e1, tau n i e2)
| Pair(e1,e2) -> Pair(tau n i e1, tau n i e2)
| Fst(e') -> Fst(tau n i e')
| Snd(e') -> Snd(tau n i e')
| Inl(e') -> Inl(tau n i e')
| Inr(e') -> Inr(tau n i e')
| Case(e',e1,e2) -> Case(tau n i e', tau n (i+1) e1, tau n (i+1) e2)
| Fix(e') -> Fix(tau n (i+1) e')
| Ifthenelse(e',e1,e2) -> Ifthenelse(tau n i e', tau n i e1, tau n i e2)
| Num _ | Arith _ | Eunit | True | False -> e

let rec sigma n e1 e2 = match e1 with
| Ind(m) -> if m < n then Ind(m) else if m = n then tau n 0 e2 else Ind(m-1)
| App(e1',e2') -> App(sigma n e1' e2, sigma n e2' e2)
| Lam(e') -> Lam(sigma (n+1) e' e2)
| Pair(e1',e2') -> Pair(sigma n e1' e2, sigma n e2' e2)
| Fst(e') -> Fst(sigma n e' e2)
| Snd(e') -> Snd(sigma n e' e2)
| Inl(e') -> Inl(sigma n e' e2)
| Inr(e') -> Inr(sigma n e' e2)
| Case(e',e1',e2') -> Case(sigma n e' e2, sigma (n+1) e1' e2, sigma (n+1) e2' e2)
| Fix(e') -> Fix(sigma (n+1) e' e2)
| Ifthenelse(e',e1',e2') -> Ifthenelse(sigma n e' e2, sigma n e1' e2, sigma n e2' e2)
| Num _ | Arith _ | Eunit | True | False -> e1

let rec isValV e = match e with
| Lam _ | True | False | Num _ | Eunit | Arith _ -> true
| Inl(e') | Inr(e') -> isValV e'
| Pair(e1,e2) -> isValV e1 && isValV e2
| _ -> false

let rec isValN e = match e with
| Lam _ | True | False | Num _ | Eunit | Arith _ | Pair _ -> true
| Inl(e') | Inr(e') -> isValN e'
| _ -> false

(* stepv : Tml.exp -> Tml.exp *)   
let rec stepv e = match e with
| Ind(i) -> raise Stuck
| Lam(e') -> raise Stuck
| App(e1,e2) -> (match e1 with
                | Lam(e') -> if isValV e2 then sigma 0 e' e2 else App(e1,stepv e2)
                | Arith(op) -> (match e2 with
                                | Pair(Num(n1),Num(n2))->(match op with
                                                          | Plus -> Num(n1+n2)
                                                          | Minus -> Num(n1-n2)
                                                          | Eq -> if n1 = n2 then True else False)
                                | _ -> App(e1,stepv e2))
                | _ -> App(stepv e1,e2))
| Pair(e1,e2) -> if isValV e1 then Pair(e1,stepv e2) else Pair(stepv e1,e2)
| Fst(e') -> if isValV e' then (match e' with | Pair(v1,v2) -> v1 | _ -> raise Stuck)
            else Fst(stepv e')
| Snd(e') -> if isValV e' then (match e' with | Pair(v1,v2) -> v2 | _ -> raise Stuck)
            else Snd(stepv e')                      
| Eunit -> raise Stuck
| Inl(e') -> Inl(stepv e')
| Inr(e') -> Inr(stepv e')
| Case(e',e1,e2) -> (match e' with
                    | Inl(v) -> if isValV v then sigma 0 e1 v else Case(stepv e',e1,e2)
                    | Inr(v) -> if isValV v then sigma 0 e2 v else Case(stepv e',e1,e2)
                    | _ -> Case(stepv e',e1,e2))
| Fix(e') -> sigma 0 e' e
| True -> raise Stuck
| False -> raise Stuck
| Ifthenelse(e',e1,e2) -> (match e' with
                          | True -> e1
                          | False -> e2
                          | _ -> Ifthenelse(stepv e',e1,e2))
| Num(n) -> raise Stuck
| Arith(op) -> raise Stuck

(* stepn : Tml.exp -> Tml.exp *)   
let rec stepn e = match e with
| Ind(i) -> raise Stuck
| Lam(e') -> raise Stuck
| App(e1,e2) -> (match e1 with
                | Lam(e') -> sigma 0 e' e2
                | Arith(op) -> (match e2 with
                                | Pair(Num(n1),Num(n2))->(match op with
                                                          | Plus -> Num(n1+n2)
                                                          | Minus -> Num(n1-n2)
                                                          | Eq -> if n1 = n2 then True else False)
                                | Pair(e1',e2') -> if isValN e1' then App(e1, Pair(e1',stepn e2')) else App(e1, Pair(stepn e1', e2'))
                                | _ -> raise Stuck)
                | _ -> App(stepn e1,e2))
| Pair(e1,e2) -> raise Stuck
| Fst(e') -> (match e' with
            | Pair(e1,e2) -> e1
            | _ -> Fst(stepn e'))
| Snd(e') -> (match e' with
            | Pair(e1,e2) -> e2
            | _ -> Snd(stepn e'))
| Eunit -> raise Stuck
| Inl(e') -> raise Stuck
| Inr(e') -> raise Stuck
| Case(e',e1,e2) -> (match e' with
                    | Inl(e'') -> sigma 0 e1 e''
                    | Inr(e'') -> sigma 0 e2 e''
                    | _ -> Case(stepn e',e1,e2))
| Fix(e') -> sigma 0 e' e
| True -> raise Stuck
| False -> raise Stuck
| Ifthenelse(e',e1,e2) -> (match e' with
                          | True -> e1
                          | False -> e2
                          | _ -> Ifthenelse(stepn e',e1,e2))
| Num(n) -> raise Stuck
| Arith(op) -> raise Stuck
