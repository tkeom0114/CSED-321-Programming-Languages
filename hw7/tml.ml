exception NotImplemented 
exception Stuck

type var = string 
type index = int
               
type tp =                                  (* types *) 
    Bool                                     (* bool *)
  | Int                                      (* int *)
  | Fun of tp * tp                           (* tp -> tp *)
  | Prod of tp * tp                          (* tp * tp *)
  | Unit                                     (* unit *)
  | Sum of tp * tp                           (* tp + tp *)

type bop =                                 (* binary operators *)
    Plus                                     (* addition *)
  | Minus                                    (* subtraction *)
  | Eq                                       (* equality *)

type texp =                                (* expressions *)
    Tvar of var                              (* variable *)
  | Tlam of var * tp * texp                  (* lambda abstraction *)
  | Tapp of texp * texp                      (* application *)
  | Tpair of texp * texp                     (* pair (e1, e2) *)
  | Tfst of texp                             (* projection fst *)
  | Tsnd of texp                             (* projection snd *)
  | Teunit                                   (* unit *)
  | Tinl of texp * tp                        (* inleft *)
  | Tinr of texp * tp                        (* inright *)
  | Tcase of texp * var * texp * var * texp  (* case e of inl x1 => e1 | inr x2 => e2 *)  
  | Tfix of var * tp * texp                  (* fixed point construct *)
  | Ttrue                                    (* boolean true *)
  | Tfalse                                   (* boolean false *)
  | Tifthenelse of texp * texp * texp        (* conditional construct *)
  | Tnum of int                              (* integer number *)
  | Tarith of bop                            (* arithmetic function *)

type exp =                                 (* exps with de Bruijn indices *)
    Ind of index                             (* variable *)
  | Lam of exp                               (* lambda abstraction *)
  | App of exp * exp                         (* application *)
  | Pair of exp * exp                        (* pair (e1, e2) *)
  | Fst of exp                               (* projection fst *)
  | Snd of exp                               (* projection snd *)
  | Eunit                                    (* unit *)
  | Inl of exp                               (* inleft *)
  | Inr of exp                               (* inright *)
  | Case of exp * exp * exp                  (* case e of inl e1 | inr e2 *)  
  | Fix of exp                               (* fixed point construct *)
  | True                                     (* boolean true *)
  | False                                    (* boolean false *)
  | Ifthenelse of exp * exp * exp            (* conditional construct *)
  | Num of int                               (* integer number *)
  | Arith of bop                             (* arithmetic functions *)


(* exp2string : Tml.exp -> string *)
let rec exp2string exp = 
  match exp with 
    Ind x -> string_of_int x
  | Lam e -> "(lam. " ^ (exp2string e) ^ ")"
  | App (e1, e2) -> "(" ^ (exp2string e1) ^ " " ^ (exp2string e2) ^ ")"
  | Pair (e1, e2) -> "(" ^ (exp2string e1) ^ "," ^ (exp2string e2) ^ ")"
  | Fst e -> "(fst " ^ (exp2string e) ^ ")"
  | Snd e -> "(snd " ^ (exp2string e) ^ ")"
  | Eunit -> "()"
  | Inl e -> "(inl " ^ (exp2string e) ^ ")"
  | Inr e -> "(inr " ^ (exp2string e) ^ ")"
  | Case (e, e1, e2) -> "(match " ^ (exp2string e) ^" with " ^ (exp2string e1) ^ " | " ^ (exp2string e2) ^ ")"
  | Fix e -> "(fix. "  ^ (exp2string e) ^ ")"
  | Ifthenelse (e, e1, e2) -> "(if " ^ (exp2string e) ^ " then " ^ (exp2string e1) ^ " else " ^ (exp2string e2) ^ ")"
  | True -> "true"  
  | False -> "false"
  | Num n -> "<" ^ (string_of_int n) ^ ">"
  | Arith Plus -> "+"  
  | Arith Minus -> "-" 
  | Arith Eq -> "="
