type tyvar = int                        (* type variable *)
type tyname = int                       (* type name *)

type ty =                               (* type *)
    | TINT                              (* int *)
    | TBOOL                             (* bool *)
    | TUNIT                             (* unit *)
    | TNAME of tyname                   (* type name *)
    | TPAIR of ty * ty                  (* type pair *)
    | TFUN of ty * ty                   (* function type *)
    | TREF of ty                        (* reference type *)
    | TVAR of tyvar                     (* type variable *)

type pat =                              (* pattern *)
    | PWILD                             (* wild card *)
    | PINT of int                       (* integer constant *)
    | PBOOL of bool                     (* boolean constant *)
    | PUNIT                             (* unit *)
    | PVAR of Tml.id                    (* variable *)
    | PCON of Tml.id                    (* constructor *)
    | PCONP of Tml.id * patty           (* constructor with an argument *)
    | PPAIR of patty * patty            (* pattern pair *)
 and patty = PATTY of pat * ty          (* pattern with type annotation *)

type dec =
    | DTYPE of Tml.tycon * tyname       (* tycon |-> tyname *)

type exp =                              (* expression *)
    | INT of int                        (* integer constant *)
    | BOOL of bool                      (* boolean constant *)
    | UNIT                              (* unit *)
    | OP of Tml.oper                    (* operator *)
    | VAR of Tml.id                     (* variable *)
    | CON of Tml.id                     (* constructor *)
    | FUN of mrule                      (* function *)
    | MATCH of expty * mrule list       (* match *)
    | APP of expty * expty              (* funtion application *)
    | PAIR of expty * expty             (* expression pair *)
    | REF of expty                      (* reference *)
    | DEREF of expty                    (* dereference *)
    | ASSIGN of expty * expty           (* assignment *)
    | LET of patty * expty * expty      (* let *)
    | LETREC of patty * expty * expty   (* let rec *)
 and expty = EXPTY of exp * ty          (* expression with type annotation *)

 and mrule = MRULE of (patty * expty)   (* pattern matching rule *)

type program = dec list * expty         (* program *)
