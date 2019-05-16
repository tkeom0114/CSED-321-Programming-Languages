open Typed

let tyname2str tn = "type_" ^ (string_of_int tn)
let tyvar2str =  string_of_int

let rec ty2str = function
    | TINT              -> "int"
    | TBOOL             -> "bool"
    | TUNIT             -> "unit"
    | TNAME tn          -> tyname2str tn
    | TPAIR (ty, ty')   -> "(" ^ (ty2str ty) ^ ", " ^ (ty2str ty') ^ ")"
    | TFUN (ty, ty')    -> "(" ^ (ty2str ty) ^ " -> " ^ (ty2str ty') ^ ")"
    | TREF ty           -> (ty2str ty) ^ " ref"
    | TVAR tv           -> "'" ^ (tyvar2str tv)

let rec pat2str = function
    | PWILD             -> "_"
    | PINT i            -> string_of_int i
    | PBOOL b           -> string_of_bool b
    | PUNIT             -> "()"
    | PVAR id           -> Print.id2str id
    | PCON id           -> Print.id2str id
    | PCONP (id, pt)    -> (Print.id2str id) ^ " " ^ (patty2str pt)
    | PPAIR (pt, pt')   -> "(" ^ (patty2str pt) ^ ", " ^ (patty2str pt') ^ ")"
and patty2str 
      (PATTY (p, ty))   =  "(" ^ (pat2str p) ^ " : " ^ (ty2str ty) ^ ")"

let dec2str
      (DTYPE (tc, tn))  =  "type " ^ (Print.tycon2str tc) ^ " : " ^ (tyname2str tn)

let rec exp2str = function
    | INT i                 -> string_of_int i
    | BOOL b                -> string_of_bool b
    | UNIT                  -> "()"
    | OP op                 -> Print.oper2str op
    | VAR id                -> Print.id2str id
    | CON id                -> Print.id2str id
    | FUN m                 -> "(fun " ^ (mrule2str m) ^ ")"
    | MATCH (et, ml)        -> "(match " ^ (expty2str et) ^ " with " ^ (String.concat " | " (List.map mrule2str ml)) ^ ")"
    | APP (et, et')         -> "(" ^ (expty2str et) ^ " " ^ (expty2str et') ^ ")"
    | PAIR (et, et')        -> "(" ^ (expty2str et) ^ ", " ^ (expty2str et') ^ ")"
    | REF et                -> "(ref " ^ (expty2str et) ^ ")"
    | DEREF et              -> "(! " ^ (expty2str et) ^ ")"
    | ASSIGN (et, et')      -> "(" ^ (expty2str et) ^ " := " ^ (expty2str et') ^ ")"
    | LET (pt, et, et')     -> "(let " ^ (letbind2str pt et) ^ " in\n" ^ (expty2str et') ^ ")"
    | LETREC (pt, et, et')  -> "(let rec " ^ (letbind2str pt et) ^ " in\n" ^ (expty2str et') ^ ")"

and expty2str 
      (EXPTY (exp, ty))     = "(" ^ (exp2str exp) ^ " : " ^ (ty2str ty) ^ ")"

and letbind2str pt et       = (patty2str pt) ^ " = " ^ (expty2str et)

and mrule2str 
      (MRULE (pt, et))      = (patty2str pt) ^ " -> " ^ (expty2str et)

let program2str (dlist, ety) =
    (String.concat "\n" (List.map dec2str dlist)) ^ 
    (match dlist with [] -> "" | _ -> "\n;;\n") ^ 
    (expty2str ety) ^ "\n"

