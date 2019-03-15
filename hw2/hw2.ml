exception NotImplemented
exception InvalidLocation
	    
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
						      
(** Recursive functions **)

let rec lconcat l = match l with
| [] -> []
| a::l' -> a@lconcat l'


let rec lfoldl f e l = match l with
| [] -> e
| x::l' -> lfoldl f (f (x,e)) l'					
			 
(** Tail recursive functions  **)

let fact n = let rec f i res = if i = n then res else f (i+1) (res*(i+1)) in f 1 1

let power x n = let rec f i res = if i = n then res else f (i+1) (res*x) in f 0 1

let fib n = let rec fib' i a b = if i = n then a else fib' (i+1) b (a+b) in fib' 0 1 1

let lfilter p l = 
  let rec lfilter' p l l1 = match l with
  | [] -> l1
  | a::l' -> if p a then lfilter' p l' (l1@[a])
             else lfilter' p l' l1
in lfilter' p l []

let ltabulate n f = 
  let rec ltabulate' i n f l = if i = n then l else ltabulate' (i+1) n f (l@[f i])
in ltabulate' 0 n f []

let rec union s t = 
  let rec exist x l = match l with
  | [] -> false
  | a::l' -> (a=x) || exist x l'
in match t with
| [] -> s
| a::t' -> if (exist a s) then union s t' else union (a::s) t'

let inorder t = 
  let rec inorder' tl accl = match tl with
  | [] -> accl
  | t::tl' -> match t with
    | Leaf a -> inorder' tl' (accl@[a])
    | Node (lt,a,rt) -> inorder' (lt::(Leaf a)::rt::tl') accl
in inorder' [t] []   

let postorder t =
  let rec postorder' tl accl = match tl with
| [] -> accl
  | t::tl' -> match t with
    | Leaf a -> postorder' tl' (a::accl)
    | Node (lt,a,rt) -> postorder' (rt::lt::tl') (a::accl)
in postorder' [t] []

let preorder t = 
  let rec preorder' tl accl = match tl with
  | [] -> accl
  | t::tl' -> match t with
    | Leaf a -> preorder' tl' (accl@[a])
    | Node (lt,a,rt) -> preorder' (lt::rt::tl') (accl@[a])
in preorder' [t] []
		       
(** Sorting in the ascending order **)

let rec quicksort l = 
  let rec split a l = match l with
  | [] -> ([],[])
  | x::l' -> if a>x then (x::fst (split a l'), (snd (split a l'))) else (fst (split a l'),x::(snd (split a l')))
in match l with
| [] -> []
| a::l' -> (quicksort (fst (split a l')))@[a]@(quicksort (snd (split a l')))

let rec split l = let f a (l,m) = (a::m,l) 
  in match l with
  | [] -> ([],[])
  | [a] -> (l,[])
  | a::l' -> f a (split l')

let rec merge l1 l2 = match (l1,l2) with
| ([],_) -> l2
| (_,[]) -> l1
| (a1::l1',a2::l2') -> if a1<a2 then a1::merge l1' l2 else a2::merge l1 l2'

let rec mergesort l = match l with
| [] -> []
| [a] -> [a]
| a::l' -> merge (mergesort (fst (split l))) (mergesort (snd (split l)))

(** Structures **)

module type HEAP = 
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a 
    val update : 'a heap -> loc -> 'a -> 'a heap
  end
    
module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict 
  end

module Heap : HEAP =
  struct
    exception InvalidLocation 
		
    type loc = int    
    type 'a heap = (loc * 'a) list  

    let empty () = []
    let allocate h v = 
      let l = List.length h + 1
      in ((l,v)::h,l)
    let dereference h l = try let (l',v') = List.find (fun (loc,v) -> loc = l) h 
      in v'
      with Not_found -> raise InvalidLocation     
    let update h l v = try let _ = List.find (fun (loc,v) -> loc = l) h 
     in List.map (fun (loc,value) -> if loc = l then (loc,v) else (loc,value)) h
     with Not_found -> raise InvalidLocation   
  end
    
module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list
			      
    let empty () = []
    let lookup d k = try let (k',v') = List.find (fun (key,value) -> key = k) d 
      in Some v'
      with Not_found -> None
    let rec delete d k = match d with
    | [] -> []
    | (key,value)::d' -> if key = k then d' else (key,value)::delete d' k
    let insert d (k,v) = try let _ = List.find (fun (key,value) -> key = k) d 
    in List.map (fun (key,value) -> if key = k then (key,v) else (key,value)) d
    with Not_found -> (k,v)::d
  end
    
module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option
			     
    let empty () = fun _ -> None
    let lookup d k = d k
    let delete d k = fun k' -> if k' = k then None else d k'
    let insert d (k,v) = fun k' -> if k' = k then Some v else d k'
  end
