
module StrMap = Map.Make(String)

(* map with string keys and value type 'a *)
type 'a env = 'a StrMap.t

(* the empty environment *)
let empty = StrMap.empty

(* returns v if env contains n |-> v; 
 * otherwise, raise Not_found *)
let lookup n env = StrMap.find n env

(* returns a map containing the same set of
 * bindings as env plus a new binding y |-> l *)
let insert y l env = StrMap.add y l env

(* returns a map with the single binding y |-> l *)
let singleton y l = StrMap.singleton y l


