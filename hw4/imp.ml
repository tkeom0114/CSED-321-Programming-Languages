
type id = string

type exp = Num  of int
         | Bool of bool
         | Var  of id
         | Add  of exp * exp
         | Sub  of exp * exp
         | Mul  of exp * exp
         | Div  of exp * exp
         | Eq   of exp * exp
         | Less of exp * exp
         | And  of exp * exp
         | Or   of exp * exp
         | Not  of exp

type stmt = Skip
          | Assign of id * exp
          | Seq    of stmt * stmt
          | If     of exp * stmt * stmt
          | If2    of exp * stmt
          | While  of exp * stmt

type program = stmt * exp

let rec exp2str = function
    | Num(i)       -> string_of_int i
    | Bool(b)      -> string_of_bool b
    | Var(n)       -> n
    | Add(e1, e2)  -> "(" ^ (exp2str e1) ^ " + " ^ (exp2str e2) ^ ")"
    | Sub(e1, e2)  -> "(" ^ (exp2str e1) ^ " - " ^ (exp2str e2) ^ ")"
    | Mul(e1, e2)  -> "(" ^ (exp2str e1) ^ " * " ^ (exp2str e2) ^ ")"
    | Div(e1, e2)  -> "(" ^ (exp2str e1) ^ " / " ^ (exp2str e2) ^ ")"
    | Eq(e1, e2)   -> "(" ^ (exp2str e1) ^ " = " ^ (exp2str e2) ^ ")"
    | Less(e1, e2) -> "(" ^ (exp2str e1) ^ " < " ^ (exp2str e2) ^ ")"
    | And(e1, e2)  -> "(" ^ (exp2str e1) ^ " && " ^ (exp2str e2) ^ ")"
    | Or(e1, e2)   -> "(" ^ (exp2str e1) ^ " || " ^ (exp2str e2) ^ ")"
    | Not(e)       -> "(! " ^ (exp2str e) ^ ")"

