
module IntMap = Map.Make(struct type t = int let compare = compare end)

type 'a store = 'a IntMap.t     (* store *)

let empty = IntMap.empty

let alloc v store = 
    let l = IntMap.cardinal store in
    (l, IntMap.add l v store)
                          
let deref l store = IntMap.find l store

let update l v store =
    if IntMap.mem l store then IntMap.add l v store else raise Not_found

