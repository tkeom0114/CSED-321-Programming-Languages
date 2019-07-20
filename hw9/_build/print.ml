open Tml

let id2str = fun s -> s
let tycon2str = fun s -> s

let rec ty2str ty = 
    match ty with 
    | TINT             -> "int"
    | TBOOL            -> "bool"
    | TUNIT            -> "unit"
    | TCON tc          -> tycon2str tc
    | TPAIR (ty, ty')  -> "(" ^ (ty2str ty) ^ " * " ^ (ty2str ty') ^ ")"
    | TFUN (ty, ty')   -> "(" ^ (ty2str ty) ^ "->" ^ (ty2str ty') ^ ")"
    | TREF ty          -> "(" ^ (ty2str ty) ^ " ref)"

let rec pat2str pat = 
    match pat with
    | PWILD            -> "_"
    | PINT i           -> string_of_int i
    | PBOOL b          -> string_of_bool b
    | PUNIT            -> "()"
    | PVAR id          -> id2str id
    | PCON id          -> id2str id
    | PCONP (id, p)    -> (id2str id) ^ " " ^ (pat2str p)
    | PPAIR (p, p')    -> "(" ^ (pat2str p) ^ ", " ^ (pat2str p') ^ ")"
    | PTPAT (p, ty)    -> (pat2str p) ^ " : " ^ (ty2str ty)

let conbinding2str conbinding = 
    match conbinding with
    | CBCON id         -> id2str id | CBTCON (id, ty)  -> (id2str id) ^ " of " ^ (ty2str ty)

let dec2str (DTYPE (tc, cblist)) = 
    "type " ^ (tycon2str tc) ^ " = " ^ (String.concat " | " (List.map conbinding2str cblist))

let oper2str op =
    match op with
    | PLUS      -> "+"
    | MINUS     -> "-"
    | MULT      -> "*"
    | EQ        -> "="
    | LESS      -> "<"

let rec exp2str exp = match exp with 
    INT i              -> string_of_int i
  | BOOL b             -> string_of_bool b
  | UNIT               -> "()"
  | OP op              -> oper2str op
  | VAR id             -> id2str id
  | CON id             -> id2str id
  | FUN m              -> "(fun " ^ (mrule2str m) ^ ")"
  | MATCH (e, ml)      -> "(match " ^ (exp2str e) ^ " with " ^ (String.concat " | " (List.map mrule2str ml)) ^ ")"
  | APP (e, e')        ->  "(" ^ (exp2str e) ^ " " ^ (exp2str e') ^ ")"
  | PAIR (e, e')       -> "(" ^ (exp2str e) ^ ", " ^ (exp2str e') ^ ")"
  | REF e              -> "(ref " ^ (exp2str e) ^ ")"
  | DEREF e            -> "(! " ^ (exp2str e) ^ ")"
  | ASSIGN (e, e')     -> "(" ^ (exp2str e) ^ " := " ^ (exp2str e') ^ ")"
  | LET (p, e, e')     -> "(let " ^ (letbind2str p e) ^ " in\n" ^ (exp2str e') ^ ")"
  | LETREC (p, e, e')  -> "(let rec " ^ (letbind2str p e) ^ " in\n" ^ (exp2str e') ^ ")"
  | TEXP (e, ty)       -> "(" ^ (exp2str e) ^ " : " ^ (ty2str ty) ^ ")"

and letbind2str pat exp = 
    (pat2str pat) ^ " = " ^ (exp2str exp)

and mrule2str (MRULE (pat, exp)) = 
    (pat2str pat) ^ " -> " ^ (exp2str exp)

(* val program2str : Ast.program -> string *)
let program2str (dlist, exp) = 
    (String.concat "\n" (List.map dec2str dlist)) ^ 
    (match dlist with [] -> "" | _ -> "\n;;\n") ^ 
    (exp2str exp) ^ "\n"

