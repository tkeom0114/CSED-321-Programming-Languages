exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum n = if n = 1 then 1 else sum (n-1) + n
let rec fac n = if n = 1 then 1 else fac (n-1) * n
let rec fib n = let rec fib' i a b = if i = n then a else fib' (i+1) b (a+b) in fib' 0 1 1
let rec gcd m n = if m = 0 then n else if n = 0 then m else if m>n then gcd (m-n) n else gcd m (n-m)
let rec max l = match l with
| [] -> 0
| a::l' ->if l'=[] then a else if a > max l' then a else max l'

let rec sum_tree t = match t with
| Leaf a -> a
| Node (lt,a,rt) -> sum_tree lt + a + sum_tree rt
let rec depth t = match t with
| Leaf a -> 0
| Node (lt,a,rt) -> max [depth lt;depth rt] + 1
let rec bin_search t x = match t with
| Leaf a -> if a = x then true else false
| Node (lt,a,rt) -> if a = x then true else if a > x then bin_search lt x else bin_search rt x
let rec preorder t = match t with
| Leaf a -> [a]
| Node (lt,a,rt) -> a::preorder lt @ preorder rt
let rec list_add l m = match (l,m) with
| ([],m) -> m
| (l,[]) -> l
| (a::l',b::m') -> a+b::list_add l' m'
let rec insert m l = match l with
| [] -> [m]
| a::l' -> if m < a then m::l else a::insert m l'
let rec insort l = match l with
| [] -> []
| a::l' -> insert a (insort l')

let rec compose f g  = (fun x -> g (f x)) 
let rec curry f x y = (fun a b -> f (a,b)) x y
let rec uncurry f x = (fun (a,b) -> f a b) x
let rec multifun f n = if n = 1 then f else compose f (multifun f (n-1))

let rec ltake l n = match l with
| [] -> []
| a::l' -> if n = 0 then [] else a::(ltake l' (n-1))
let rec lall f l = match l with
| [] -> true
| a::l' -> if f a then lall f l' else false
let rec lmap f l = match l with
| [] -> []
| a::l' -> (f a)::(lmap f l')
let rec lrev l = match l with
| [] -> []
| a::l' -> (lrev l') @ [a]
let rec lzip l m = match (l,m) with
| ([],m) -> []
| (l,[]) -> []
| (a::l',b::m') -> (a,b)::(lzip l' m')
let rec split l = let f a (l,m) = (a::m,l) 
in match l with
| [] -> ([],[])
| [a] -> (l,[])
| a::l' -> f a (split l')
let rec cartprod l m = match (l,m) with
| (_,[]) -> []
| ([],_) -> []
| ([a],l) -> lmap (fun x -> (a,x)) l
| (a::l',m) -> (cartprod [a] m) @ (cartprod l' m) 
