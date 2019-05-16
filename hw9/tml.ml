type id = string                            (* <identifier> *)
type tycon = string                         (* <type constructor> *)

type ty =
    | TINT                                  (* int *)
    | TBOOL                                 (* bool *)
    | TUNIT                                 (* unit *)
    | TCON of tycon                         (* tycon *)
    | TPAIR of ty * ty                      (* (ty * ty) *)
    | TFUN of ty * ty                       (* (ty -> ty) *)
    | TREF of ty                            (* ty ref *)

type pat =
    | PWILD                                 (* _ *)
    | PINT of int                           (* num *)
    | PBOOL of bool                         (* true | false *)
    | PUNIT                                 (* () *)
    | PVAR of id                            (* variable *)
    | PCON of id                            (* constructor *)
    | PCONP of id * pat                     (* constructor pat *)
    | PPAIR of pat * pat                    (* (pat, pat) *)
    | PTPAT of pat * ty                     (* (pat : ty) *)

type conbinding =
    | CBCON of id                           (* constructor *)
    | CBTCON of id * ty                     (* constructor of ty *)

type dec =
    | DTYPE of tycon * conbinding list      (* type tycon = conbind *)

type oper =
    | PLUS                                  (* + *)
    | MINUS                                 (* - *)
    | MULT                                  (* * *)
    | EQ                                    (* = *)
    | LESS                                  (* < *)

type exp =
    | INT of int                            (* num *)
    | BOOL of bool                          (* true | false *)
    | UNIT                                  (* () *)
    | OP of oper                            (* operator *)
    | VAR of id                             (* variable *)
    | CON of id                             (* constructor *)
    | FUN of mrule                          (* fun *)
    | MATCH of exp * mrule list             (* match *)
    | APP of exp * exp                      (* exp exp *)
    | PAIR of exp * exp                     (* (exp, exp) *)
    | REF of exp                            (* ref exp *)
    | DEREF of exp                          (* ! exp *)
    | ASSIGN of exp * exp                   (* exp := exp *)
    | LET of pat * exp * exp                (* let pat = exp in exp *)
    | LETREC of pat * exp * exp             (* let rec pat = exp in exp *)
    | TEXP of exp * ty                      (* (exp : ty) *)

 and mrule = 
   MRULE of pat * exp                       (* pat => exp *)

type program = dec list * exp               (* program *)

