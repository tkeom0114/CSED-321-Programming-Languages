exception NotImplemented
exception TypingError


open Tml
open Typed
module StrMap = Map.Make(String)
type 'a cond = 'a StrMap.t

type tidcon = Typed.tyname cond
type varcon = (Typed.tyvar list * Typed.ty) cond
type typcon = tidcon * varcon
type sub = ID | SUB of Typed.ty * Typed.ty | COM of sub * sub
type teq = Typed.ty * Typed.ty

let empty = StrMap.empty
(* returns v if env contains n |-> v; 
 * otherwise, raise Not_found *)
let f x y1 y2 = Some(y2) (*StrMap.union f con1 con2*)

let freshVar = ref 0
let getFreshVar _ = 
  let _ = freshVar := !freshVar + 1
  in (!freshVar:Typed.tyvar)
let rec getMultiFVar n = if n = 0 then [] else getFreshVar()::getMultiFVar (n-1)

let freshName = ref 0
let getFreshName _ = 
  let _ = freshName := !freshName + 1
  in (!freshName:Typed.tyname)

let alloc v con = 
  let var = StrMap.cardinal con in
  (var, StrMap.add v var con)

let lookup var con = StrMap.find var con

let insert y var con = StrMap.add y var con

let rec union s1 s2 = match s2 with
| [] -> s1
| a::s' -> if (List.mem a s1) then union s1 s' else union (s1@[a]) s'

let rec its s1 s2 = match s2 with
| [] -> []
| a::s' -> if (List.mem a s1) then a::(its s1 s') else its s1 s'

let diff s1 s2 = List.filter (fun x -> not (List.mem x s2)) s1

let rec fvar t = match t with
| Typed.TINT | Typed.TBOOL | Typed.TUNIT | Typed.TNAME(_) -> []
| Typed.TVAR(alpha) -> [alpha]
| Typed.TREF(t') -> fvar t'
| Typed.TFUN(t1,t2) | Typed.TPAIR(t1,t2) -> union (fvar t1) (fvar t2)

let rec fte te = raise NotImplemented

let fve (ve:varcon) = StrMap.fold (fun k (vlist,t) res -> union (diff (fvar t) vlist) res) ve []

let ftc (tc:typcon) = fve (snd tc)

let rec ty2tpty (tc:typcon) t = match t with
| Tml.TINT -> Typed.TINT
| Tml.TBOOL -> Typed.TBOOL
| Tml.TUNIT -> Typed.TUNIT
| Tml.TCON(c) -> Typed.TNAME(lookup c (fst tc))
| Tml.TPAIR(t1,t2) -> Typed.TPAIR(ty2tpty tc t1,ty2tpty tc t2)
| Tml.TFUN(t1,t2) -> Typed.TFUN(ty2tpty tc t1,ty2tpty tc t2)
| Tml.TREF(t') -> Typed.TREF(ty2tpty tc t')

let rec pat2bind (tc:typcon) (p:Tml.pat) = match p with
| Tml.PWILD -> (Typed.TVAR(getFreshVar ()), empty)
| Tml.PINT(_) -> (Typed.TINT, empty) 
| Tml.PBOOL(_) -> (Typed.TBOOL, empty) 
| Tml.PUNIT -> (Typed.TUNIT, empty) 
| Tml.PCON(c) -> (Typed.TNAME(StrMap.find c (fst tc)), empty)
| Tml.PVAR(v) -> let t = Typed.TVAR(getFreshVar()) in (t,(StrMap.singleton v ([],t)))
| Tml.PCONP(c,p') -> (Typed.TNAME(StrMap.find c (fst tc)), snd(pat2bind tc p'))
| Tml.PPAIR(p1,p2) -> let (t1,ve1) = pat2bind tc p1 in let (t2,ve2) = pat2bind tc p2 in (Typed.TPAIR(t1,t2),StrMap.union f ve1 ve2)
| Tml.PTPAT(p',t') -> let (t,ve) = pat2bind tc p' in if t = ty2tpty tc t' then (t,ve) else raise TypingError

let rec pat2patty (tc:typcon) p = match p with 
| Tml.PWILD -> PATTY(Typed.PWILD, Typed.TVAR(getFreshVar()))
| Tml.PINT(n) -> PATTY(Typed.PINT(n),Typed.TINT)
| Tml.PBOOL(b) -> PATTY(Typed.PBOOL(b),Typed.TBOOL)
| Tml.PCON(c) -> PATTY(Typed.PCON(c),Typed.TNAME(StrMap.find c (fst tc)))
| Tml.PUNIT -> PATTY(Typed.PUNIT,Typed.TUNIT)
| Tml.PVAR(v) -> PATTY(Typed.PVAR(v),snd(StrMap.find v (snd tc)))
| Tml.PCONP(c,p') -> (match snd (StrMap.find c (snd tc)) with 
                      | Typed.TFUN(ty',Typed.TNAME(tn))-> Typed.PATTY(Typed.PCONP(c,pat2patty tc p'),Typed.TNAME(StrMap.find c (fst tc)))
                      | _ -> raise TypingError) 
| Tml.PPAIR(p1,p2) -> let PATTY(pty1,t1) = pat2patty tc p1 in let PATTY(pty2,t2) = pat2patty tc p2 in 
                      PATTY(Typed.PPAIR(PATTY(pty1,t1),PATTY(pty2,t2)),Typed.TPAIR(t1,t2))
| Tml.PTPAT(p',t') -> let PATTY(pty,t) = pat2patty tc p' in if ty2tpty tc t' = t then PATTY(pty,t) else raise TypingError

let rec trade t b a = match t with
| Typed.TINT | Typed.TBOOL | Typed.TUNIT | Typed.TNAME(_) -> t
| Typed.TVAR(v) -> if v=a then Typed.TVAR(b) else t
| Typed.TPAIR(t1,t2) -> Typed.TPAIR( trade t1 b a, trade t2 b a)
| Typed.TFUN(t1,t2) -> Typed.TFUN ( trade t1 b a, trade t2 b a)
| Typed.TREF(t') -> Typed.TREF(trade t' b a)

let rec subv t a alpha = match t with
| Typed.TINT | Typed.TBOOL | Typed.TUNIT | Typed.TNAME(_) -> t
| Typed.TVAR(v) -> if t=alpha then a else t
| Typed.TPAIR(t1,t2) -> Typed.TPAIR(subv t1 a alpha, subv t2 a alpha)
| Typed.TFUN(t1,t2) -> Typed.TFUN (subv t1 a alpha, subv t2 a alpha)
| Typed.TREF(t') -> Typed.TREF(subv t' a alpha)

let rec subty sub ((fvl,t):(Typed.tyvar list * Typed.ty)) = match sub with 
| ID -> (fvl,t)
| SUB(a, alpha) -> let confb = its (fvar a) fvl in
                   let n = List.length confb in 
                   let newb = getMultiFVar n in 
                   let fvl' = union (diff fvl confb) newb in
                   let t' = List.fold_left2 trade t newb confb in
                 (match fvl' with
                  | [] -> (match t' with 
                          | Typed.TFUN(b1,b2)-> let (s1,b1') = subty sub ([],b1) in
                                                let (s2,b2') = subty sub ([],b2) in
                                                (fvl,Typed.TFUN(b1',b2')) 
                          | _ -> (fvl,subv t' a alpha)) 
                  | fv::fvl'' -> if Typed.TVAR(fv) = alpha then (fvl',subv t' a alpha)  else let (fvl''',t'') =  subty sub(fvl'',t') in (fv::fvl''',t''))
| COM(sub1,sub2) -> subty sub1 (subty sub2 (fvl,t))

let rec sublist bl al = match (bl,al) with
| (b1::bl',a1::al') -> COM(sublist bl' al', SUB(Typed.TVAR(b1),Typed.TVAR(a1)))
| _ -> ID

let subtc sub ((te,ve):typcon) = ((te,StrMap.map (fun (fvl,t) -> subty sub (fvl,t)) ve):typcon)

let rec subteqs sub teqs = match teqs with
| [] -> []
| (a,b)::teqs' -> (snd (subty sub ([],a)),snd (subty sub ([],b)))::(subteqs sub teqs')

let rec unify (teqs:teq list) = match teqs with
| [] -> ID
| (a,b)::teqs' -> (match (a,b) with
                  | (Typed.TFUN(a1,a2),Typed.TFUN(b1,b2)) | (Typed.TPAIR(a1,a2),Typed.TPAIR(b1,b2))-> unify ((a2,b2)::(a1,b1)::teqs')
                  | (Typed.TVAR(tv),t) | (t,Typed.TVAR(tv)) -> if Typed.TVAR(tv) = t then unify teqs' 
                                                              else if not (List.mem tv (fvar t)) then let sub = SUB(t,Typed.TVAR(tv)) in COM(unify (subteqs sub teqs'),sub)
                                                              else raise TypingError
                  | _ -> if a = b then unify teqs' else raise TypingError)

let gen (fvl,v) tc = (union fvl (diff (fvar v) (ftc tc)),v)

let closure (ve:varcon) (tc:typcon) = ((StrMap.map (fun (fvl,v) -> gen (fvl,v) tc) ve):varcon)

let rec wAl ((te,ve):typcon) e = match e with 
| Tml.INT(n) -> (ID,Typed.INT(n),Typed.TINT)
| Tml.BOOL(b) -> (ID,Typed.BOOL(b),Typed.TBOOL)
| Tml.UNIT -> (ID,Typed.UNIT,Typed.TUNIT)                        
| Tml.OP(op) ->(match op with
                | PLUS | MINUS | MULT -> (ID,Typed.OP(op),Typed.TFUN(Typed.TPAIR(Typed.TINT,Typed.TINT),Typed.TINT))
                | EQ | LESS -> (ID,Typed.OP(op),Typed.TFUN(Typed.TPAIR(Typed.TINT,Typed.TINT),Typed.TBOOL)))  
| Tml.VAR(v) -> let (vlist,t) = (try(StrMap.find v ve) with Not_found -> raise TypingError) in 
                let n = List.length vlist in
                let beta = getMultiFVar n in
                (ID,Typed.VAR(v),snd (subty (sublist beta vlist) (vlist,t)))
| Tml.CON(c) -> let tn = (try(StrMap.find c te) with Not_found -> raise TypingError) in (ID, Typed.CON(c), Typed.TNAME(tn))
| Tml.FUN(Tml.MRULE(p,e')) -> let (t'',ve') = pat2bind (te,ve) p  in 
                              let (s',e',t') = wAl (te,StrMap.union f ve ve') e' in 
                              let t = snd (subty s' ([],t'')) in
                              (s', Typed.FUN(Typed.MRULE((pat2patty (subtc s' (te,ve)) p), EXPTY(e',t'))),Typed.TFUN(t,t'))
| Tml.MATCH(e',mrlist) -> raise NotImplemented


| Tml.APP(e1, e2) ->  let (s1,e1,t1) = wAl (te,ve) e1 in
                      let (s2,e2,t2) = wAl (subtc s1 (te,ve)) e2 in
                      let alpha = Typed.TVAR(getFreshVar ()) in
                      let s3 = unify [(t1,Typed.TFUN(snd (subty s2 ([],t2)),alpha))] in
                      let s = COM(COM(s3,s2),s1)in
                      (s, Typed.APP(Typed.EXPTY(e1, t1),Typed.EXPTY(e2, t2)) ,snd (subty s3 ([],alpha)))
| Tml.PAIR(e1,e2) ->  let (s1,e1,t1) = wAl (te,ve) e1 in
                      let (s2,e2,t2) = wAl (subtc s1 (te,ve)) e2 in
                      (COM(s2,s1), Typed.PAIR(Typed.EXPTY(e1,t1),Typed.EXPTY(e2,t2)) ,Typed.TPAIR(t1,t2))
| Tml.REF(e') -> let (s,tye,t) = wAl (te,ve) e' in (s,Typed.REF(Typed.EXPTY(tye,t)),Typed.TREF(t))
| Tml.DEREF(e') -> let (s,tye,t) = wAl (te,ve) e' in (match t with | Typed.TREF(t') -> (s,Typed.DEREF(Typed.EXPTY(tye,t)),t') | _ -> raise TypingError)
| Tml.ASSIGN(e1,e2) ->  let (s1,e1,t1) = wAl (te,ve) e1 in
                        let (s2,e2,t2) = wAl (subtc s1 (te,ve)) e2 in
                        (match t1 with | Typed.TREF(t) -> if t = t2 then (COM(s2,s1),Typed.ASSIGN(EXPTY(e1,t1),EXPTY(e2,t2)),Typed.TUNIT) else raise TypingError | _ -> raise TypingError)
| Tml.LET(p,e1,e2) -> let (t'',ve') = pat2bind (te,ve) p  in 
                      let (s1,e1,t1) = wAl (te,ve) e1 in 
                      let uve = closure ve' (te,ve) in
                      let (s2,e2,t2) = wAl (te, StrMap.union f ve uve ) e2 in
                      (COM(s2,s1), Typed.LET(pat2patty (subtc s2 (te, StrMap.union f ve uve )) p,EXPTY(e1,t1),EXPTY(e2,t2)),t2)
| Tml.LETREC(p,e1,e2) -> let (t'',ve') = pat2bind (te,ve) p  in 
                         let (s1,e1,t1) = wAl (te, StrMap.union f ve ve') e1 in 
                         let uve = closure ve' (te,ve) in
                         let (s2,e2,t2) = wAl (te, StrMap.union f ve uve ) e2 in
                         (COM(s2,s1), Typed.LETREC(pat2patty (subtc s2 (te, StrMap.union f ve uve )) p,EXPTY(e1,t1),EXPTY(e2,t2)),t2)
| Tml.TEXP(e',t') -> let (s,tye,t) = wAl (te,ve) e' in if t = ty2tpty (te,ve) t' then (s,tye,t) else raise TypingError


let conbinding (tc:typcon) c tn = let (te,ve) = tc in match c with
| Tml.CBCON(v) -> (te,StrMap.add v ([],Typed.TNAME(tn)) ve)
| Tml.CBTCON(v,t) -> let t' = ty2tpty tc t in (te, StrMap.add v ([],Typed.TFUN(t',Typed.TNAME(tn))) ve)

let convtdec (tc:typcon) (dec:Tml.dec) = let (te,ve) = tc in let Tml.DTYPE(tcon,clist) = dec in let tn = try (StrMap.find tcon te) with Not_found -> getFreshName () in
let (te':tidcon) = StrMap.add tcon tn te in
(List.fold_left (fun con c -> conbinding con c tn) (te',ve) clist, Typed.DTYPE(tcon,tn))

let rec convtdeclist (tc:typcon) (dlist:Tml.dec list) = match dlist with
| [] -> (tc,[])
| dec::dlist' -> let ((te',ve'),dec') = convtdec tc dec in 
                 let ((te'',ve''),dlist'') = convtdeclist (te',ve') dlist' in
                 ((StrMap.union f te' te'',StrMap.union f ve' ve''),dec'::dlist'') 

(* tprogram : Tml.program -> Typed.program *)

let tprogram (dlist, exp) = let (con,dlist') = convtdeclist (empty,empty) dlist in let (s,e,t) = wAl con exp in (dlist', Typed.EXPTY(e,t))
