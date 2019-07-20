exception AstValidateError

open Tml

module StrSet = Set.Make(String)

(* vpat : Ast.pat -> Ast.pat *)
let vpat pat =
    let rec vpat' s pat = 
        match pat with
        | PWILD            -> s
        | PINT n           -> s
        | PBOOL b          -> s
        | PUNIT            -> s
        | PVAR v           -> if StrSet.mem v s then raise AstValidateError else StrSet.add v s
        | PCON c           -> s
        | PCONP (_, p)     -> vpat' s p
        | PPAIR (p1, p2)   -> vpat' (vpat' s p1) p2
        | PTPAT (p, _)     -> vpat' s p in
    let _ = vpat' StrSet.empty pat in
    pat
    
(* vconbind : Ast.conbinding list -> Ast.conbinding list *)
let vconbind conbind =
    let rec vconbind' s l = 
        match l with 
        | [] -> s
        | (CBCON c) :: conbind -> 
                if StrSet.mem c s 
                then raise AstValidateError 
                else vconbind' (StrSet.add c s) conbind
        | (CBTCON (c, t)) :: conbind -> 
                if StrSet.mem c s 
                then raise AstValidateError 
                else vconbind' (StrSet.add c s) conbind in
    let _ = vconbind' StrSet.empty conbind in
    conbind

(* vdec : Ast.dec -> Ast.dec *)
let vdec (DTYPE (tc, cblist)) = DTYPE (tc, vconbind cblist)

(* vexp : Ast.exp -> Ast.exp *)
let rec vexp exp = 
    match exp with
    | INT n                -> INT n
    | BOOL b               -> BOOL b
    | UNIT                 -> UNIT
    | OP o                 -> OP o
    | VAR v                -> VAR v
    | CON c                -> CON c
    | FUN mrule            -> FUN (vmrule mrule)
    | MATCH (e, mlist)     -> MATCH (vexp e, List.map vmrule mlist)
    | APP (e1, e2)         -> APP (vexp e1, vexp e2)
    | PAIR (e1, e2)        -> PAIR (vexp e1, vexp e2)
    | REF e                -> REF (vexp e)
    | DEREF e              -> DEREF (vexp e)
    | ASSIGN (e1, e2)      -> ASSIGN (vexp e1, vexp e2)
    | LET (p, e1, e2)      -> vlet p e1 e2
    | LETREC (p, e1, e2)   -> vletrec p e1 e2
    | TEXP (e, ty)         -> TEXP (vexp e, ty)

and vlet pat exp exp' =
    match pat with
    | PBOOL _  -> raise AstValidateError
    | _        -> LET (vpat pat, vexp exp, vexp exp')

and vletrec pat exp exp' =
    match (pat, exp) with
    | (PVAR _, FUN _)    -> LETREC (vpat pat, vexp exp, vexp exp')
    | _                  -> raise AstValidateError

(* vmrule : Ast.mrule -> Ast.mrule *)
and vmrule (MRULE (pat, exp)) = MRULE (vpat pat, vexp exp)

(* vprogram : Ast.program -> Ast.program *)
let vprogram (dlist, exp) = (List.map vdec dlist, vexp exp)

