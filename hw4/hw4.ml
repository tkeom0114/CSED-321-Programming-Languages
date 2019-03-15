open Imp

exception NotImplemented

exception RuntimeError of string

type state = unit     (* dummy type, to be chosen by students *)

(* emptystate : state *)
let emptystate = raise NotImplemented

(* eval : state -> exp -> exp *)
let eval _ _ = raise NotImplemented

(* exec : state -> stmt -> state *)
let exec _ _ = raise NotImplemented

(* run : program -> exp *)
let run (s,e) = eval (exec emptystate s) e

