exception NotImplemented 
exception Stuck
exception NotConvertible

open Tml
open Env
open Store
open Print

type location = int

(* environment *)
type env = location Env.env

(*********************************************
 * Complete the definition of the value type
 *)
type value =
     | CLOSURE of mrule * env
     | VBOOL of bool
     | VINT of int
     | VPAIR of value * value
     | VUNIT
     | VCON of id
     | VCONP of id * value
     | VOP of oper
     | VLOC of location


(* environment *)
type store = value Store.store


(* *)

(*********************************************
 * Complete the definition of the label type
 *)
type label = 
    | E of exp                          (* exp *)
    | V of value                        (* value *)
    | LAPP of exp                       (* hole exp *)
    | LFUN of value                     (* closure hole *)
    | LOP of  oper                      (* op hole(pair of exp) *)
    | LMATCH of mrule list              (* match hole with ... *)
    | LPAIRE of exp                     (* (hole, exp) *)
    | LPAIRV of value                   (* (value, hole) *) 
    | LREF                              (* ref hole *)
    | LDREF                             (* ! hole *)
    | LASSIGNE of exp                   (* hole := exp *)
    | LASSIGNL of location              (* loc := hole *)
    | LLET of pat * exp                 (* let p = hole in exp*)
    | LLETREC of pat * exp                 (* let rec p = hole in exp*)
    | RESTORE of env                    (* restore(env)*)

(* configuration *)
type config = label list * env * store

(* initial configuration *)
let initial exp = ([E(exp)], Env.empty, Store.empty) (* let initial exp = ([E(exp)], Env.empty, Store.empty)*)

(* final (value) configuration *)
let final config = match config with | ([V(v)],_,_) -> Some v | _ -> None


(*********************************************
 * value2exp : value -> Tml.exp
 * WARNING : If you give wrong implementation of this function,
 *           you wiil receive no credit for the entire problem!  *)
let rec value2exp v = match v with
| VBOOL(b) -> BOOL(b)
| VINT(n) -> INT(n)
| VUNIT -> UNIT
| VPAIR(v1,v2) -> PAIR(value2exp v1, value2exp v2)
| VCON(c) -> CON(c)
| VCONP(c,v') -> APP(CON(c), value2exp v')
| VOP(op) -> OP(op)
| _ -> raise NotConvertible

(*********************************************
 * config2str : config -> string
 * You may modify this function for debugging your code *)
let rec value2str v = match v with
| CLOSURE(MRULE(p,e),en) -> "["^ Print.pat2str p ^"->" ^  Print.exp2str e ^"]"
| VLOC(l) -> "loc " ^ string_of_int l
| _ -> Print.exp2str (value2exp v)

 let rec label2str l = match l with
 | E(e) -> Print.exp2str e
 | V(v) -> value2str v
 | LAPP(e) -> "[] " ^ Print.exp2str e
 | LFUN(cl) -> value2str cl ^ " []"
 | LOP(op) -> Print.oper2str op
 | LMATCH(mrlist) -> ""
 | LPAIRE(e) -> "([]," ^ Print.exp2str e ^ ")"
 | LPAIRV(v) -> "("^ value2str v ^  ",[])"
 | LREF -> "ref []"
 | LDREF -> "! []"
 | LASSIGNE(e) -> ""             (* hole := exp *)
 | LASSIGNL(l)  -> ""            (* loc := hole *)
 | LLET(p,e)  -> "let " ^ Print.pat2str p ^ " = [] in " ^ Print.exp2str e               (* let p = hole in exp*)
 | LLETREC(p,e)  -> "let rec " ^ Print.pat2str p ^ " = [] in " ^ Print.exp2str e              (* let rec p = hole in exp*)
 | RESTORE(en) -> "restore"
 
 let rec config2str config = 
     let rec llist2str llist = (match llist with
                             | [] -> ""
                             | l::llist' -> label2str l ^ " /-> " ^ llist2str llist')
 in match config with 
 | (llist,_,_) -> llist2str llist

(*********************************************
 * step : config -> config
 * Return the next configuration; raise Stuck if not possible
 *)
let pattbind v p en st = 
    let rec patt v p en st sub = match p with
    | PWILD -> (true,en,st,sub)
    | PINT(n) -> (match v with
                | VINT(n') -> (n=n',en,st,sub)
                | _ -> (false,en,st,sub))
    | PBOOL(b) -> (match v with
                | VBOOL(b') -> (b=b',en,st,sub)
                | _ -> (false,en,st,sub))                  
    | PUNIT -> (match v with
                | VUNIT -> (true,en,st,sub)
                | _ -> (false,en,st,sub))
    | PVAR(x) -> (if List.mem x sub then raise Stuck 
                else try(v = Store.deref (Env.lookup x en) st,en,st,sub) with Not_found -> (true,Env.insert x (fst (Store.alloc v st)) en,snd (Store.alloc v st),x::sub))
    | PCON(c) -> (match v with
                | VCON(c') -> (c=c',en,st,sub)
                | _ -> (false,en,st,sub))
    | PCONP(c,p') -> (match v with
                    | VCONP(c',v') -> patt v' p' en st sub
                    | _ -> (false,en,st,sub))
    | PPAIR(p1,p2) -> (match v with
                    | VPAIR(v1,v2) -> (match patt v1 p1 en st sub with
                                    | (able,en',st',sub') -> if able then patt v2 p2 en' st' sub' else (false,en,st,sub))
                    | _ -> (false,en,st,sub))
    | PTPAT(p',t) -> patt v p' en st sub
in match patt v p en st [] with
| (able, en',st',sub) -> if able then (true,en',st') else (false,en,st)

let rec step config = match config with
| (llist,en,st) -> (match llist with
                | [] -> raise Stuck
                | l::llist' -> (match l with
                            | V(v) -> (match llist' with
                                    | [] -> raise Stuck
                                    | l'::llist'' -> (match l' with
                                                    | LAPP(e) -> (E(e)::LFUN(v)::llist'',en,st)
                                                    | LFUN(cl) -> (match cl with
                                                                | CLOSURE(MRULE(p,e),en') -> (match pattbind v p en' st with
                                                                                            | (true,en'',st') -> (E(e)::RESTORE(en)::llist'',en'',st')
                                                                                            | _ -> raise Stuck)
                                                                | VOP(op) -> (V(v)::LOP(op)::llist'',en,st)
                                                                | VCON(c) -> (V(VCONP(c,v))::llist'',en,st)
                                                                | _ -> raise Stuck)
                                                    | LOP(op) -> (match v with
                                                                | VPAIR(VINT(v1),VINT(v2)) -> (match op with
                                                                                            | PLUS -> (V(VINT(v1+v2))::llist'',en,st)
                                                                                            | MINUS -> (V(VINT(v1-v2))::llist'',en,st)
                                                                                            | MULT -> (V(VINT(v1-v2))::llist'',en,st)
                                                                                            | EQ -> (V(VBOOL(v1=v2))::llist'',en,st)
                                                                                            | LESS -> (V(VBOOL(v1<v2))::llist'',en,st))
                                                                | _ -> raise Stuck)
                                                    | LMATCH(mrlist) -> (match mrlist with 
                                                                        | [] -> raise Stuck
                                                                        | MRULE(p,e)::mrlist' -> (match pattbind v p en st with
                                                                                                        | (true,en',st') -> (E(e)::RESTORE(en)::llist'',en',st')
                                                                                                        | _ -> (V(v)::LMATCH(mrlist')::llist'',en,st)))
                                                    | LPAIRE(e) -> (E(e)::LPAIRV(v)::llist'',en,st)
                                                    | LPAIRV(v') -> (V(VPAIR(v',v))::llist'',en,st)
                                                    | LREF -> (V(VLOC(fst(Store.alloc v st)))::llist'',en,snd (Store.alloc v st))
                                                    | LDREF -> (match v with
                                                                | VLOC(l) -> (V(Store.deref l st)::llist'',en,st)  
                                                                | _ -> raise Stuck)
                                                    | LASSIGNE(e) -> (match v with
                                                                    | VLOC(l) -> (E(e)::LASSIGNL(l)::llist'',en,st)
                                                                    | _ -> raise Stuck)
                                                    | LASSIGNL(l) -> (try(V(VUNIT)::llist'',en, Store.update l v st) with Not_found -> raise Stuck)
                                                    | LLET(p,e) -> (match pattbind v p en st with
                                                                | (true,en',st') -> (E(e)::RESTORE(en)::llist'',en',st')
                                                                | _ -> raise Stuck)
                                                    | LLETREC(p,e) -> (match (v,p) with   (* 미완성*)
                                                                    | (CLOSURE(mr,en'),PVAR(x))
                                                                    -> let alloc' = Store.alloc v st 
                                                                        in (E(e)::RESTORE(en)::llist'',Env.insert x (fst alloc') en, Store.update (fst alloc') (CLOSURE(mr,(Env.insert x (fst alloc') en))) (snd alloc'))
                                                                    | _ -> raise Stuck)
                                                    | RESTORE(en') -> (V(v)::llist'',en',st)
                                                    | _ -> raise Stuck))
                            | E(e) -> (match e with
                                    | INT(n)  -> (V(VINT(n))::llist',en,st)
                                    | BOOL(b) -> (V(VBOOL(b))::llist',en,st)
                                    | UNIT    -> (V(VUNIT)::llist',en,st)
                                    | OP(op)  -> (V(VOP(op))::llist',en,st)
                                    | VAR(x)  -> (try (V(Store.deref (Env.lookup x en) st)::llist',en,st) with Not_found -> raise Stuck)
                                    | CON(c)  -> (V(VCON(c))::llist',en,st)
                                    | FUN(mr) -> (V(CLOSURE(mr,en))::llist',en,st)           
                                    | MATCH(e', mrlist) -> (E(e')::LMATCH(mrlist)::llist',en,st)
                                    | APP(e1,e2)    -> (E(e1)::LAPP(e2)::llist',en,st)
                                    | PAIR(e1,e2)   -> (E(e1)::LPAIRE(e2)::llist',en,st)
                                    | REF(e')       -> (E(e')::LREF::llist',en,st)
                                    | DEREF(e')     -> (E(e')::LDREF::llist',en,st)
                                    | ASSIGN(e1,e2) -> (E(e1)::LASSIGNE(e2)::llist',en,st)
                                    | LET(p,e1,e2)  -> (E(e1)::LLET(p,e2)::llist',en,st)
                                    | LETREC(p,e1,e2) -> (E(e1)::LLETREC(p,e2)::llist',en,st)
                                    | TEXP(e',t) -> (E(e')::llist',en,st))
                            | _ -> raise Stuck
                            ))
                            