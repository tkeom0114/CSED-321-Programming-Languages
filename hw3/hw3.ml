open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y 
end

(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool 
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = x || y  
  let ( ** ) x y = x && y
  let (==) x y = x = y
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = elem list

  exception VectorIllegal

  let create x = match x with
  | [] -> raise VectorIllegal
  | _ -> x
  let to_list x = x
  let dim (x:t) = List.length x
  let rec nth (x:t) n = if (n < (dim x) && n >= 0) then List.nth x n
  else raise VectorIllegal
  let (++) x y = if (dim x) = (dim y) then List.map2 Scal.(++) x y
  else raise VectorIllegal
  let (==) x y = if (dim x) = (dim y) then let f res a b = res && (Scal.(==) a b)
  in List.fold_left2 f true x y
  else raise VectorIllegal
  let innerp x y = if (dim x) = (dim y) then let f res a b = Scal.(++) res (Scal.( ** ) a b)
  in List.fold_left2 f Scal.zero x y
  else raise VectorIllegal
end

(* Problem 1-3 *)
(* Matrices *)



module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  module Vec = VectorFn (Scal)
  type elem = Scal.t
  type t = Vec.t list

  exception MatrixIllegal

  let create t = match t with 
  | [] -> raise MatrixIllegal
  | _ -> List.map (fun l -> if List.length l = List.length t then  Vec.create l else raise MatrixIllegal) t 
  let init n f = let rec init' n f res = if n = 0 then res else init' (n-1) f ((f (n-1))::res) in init' n f []
  let identity n = if n <= 0 then raise MatrixIllegal else init n (fun k -> Vec.create(init n (fun i -> if i=k then Scal.one else Scal.zero)))
  let dim t = List.length t
  let nth k v = Vec.nth v k
  let transpose t = init (dim t) (fun k -> Vec.create (List.map (nth k) t))
  let to_list t = List.map (fun v -> Vec.to_list v) t
  let get t m n =if (m >= 0 && m < dim t) && (n >= 0 && n < dim t) then Vec.nth (List.nth t m) n
  else raise MatrixIllegal
  let (++) t s = if dim t = dim s then List.map2 (Vec.(++)) t s
  else raise MatrixIllegal
  let ( ** ) t s = if dim t = dim s then init (dim t) (fun m-> Vec.create (init (dim s) (fun n -> Vec.innerp (List.nth t m) (List.nth (transpose s) n))))
  else raise MatrixIllegal
  let (==) t s = if (dim t) = (dim s) then let f res a b = res && (Vec.(==) a b)
  in List.fold_left2 f true t s
  else raise MatrixIllegal
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure t = let rec closure' t res = 
    if (Mat.(==) res (Mat.(++) (Mat.identity (Mat.dim t)) (Mat.( ** ) t res) )) then res 
    else closure' t (Mat.(++) (Mat.identity (Mat.dim t)) (Mat.( ** ) t res) )
  in closure' t (Mat.identity (Mat.dim t))
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach t = try BoolMat.to_list (BoolMatClosure.closure (BoolMat.create t))
              with _ -> raise IllegalFormat

let al = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = -1            (* Dummy value : Rewrite it! *)
  let one = 0               (* Dummy value : Rewrite it! *)

  let (++) a b = if(a = zero || b = zero) then max a b  else min a b
  let ( ** ) a b = if (a = zero || b = zero) then zero else a+b
  let (==) a b = a = b
end

(* .. Write some code here .. *)

module DistMat = MatrixFn (Distance)
module DistMatClosure = ClosureFn (DistMat)

let distance t = try DistMat.to_list (DistMatClosure.closure (DistMat.create t))
            with _ -> raise IllegalFormat

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0              (* Dummy value : Rewrite it! *)
  let one = -1              (* Dummy value : Rewrite it! *)
 
  let (++) a b = if (a = one || b = one) then one else max a b
  let ( ** ) a b = if (a = one || b = one) then max a b else min a b
  let (==) a b = a =b
end

(* .. Write some code here .. *)

module WeightMat = MatrixFn (Weight)
module WeightMatClosure = ClosureFn (WeightMat)

let weight t = try WeightMat.to_list (WeightMatClosure.closure (WeightMat.create t))
          with _ -> raise IllegalFormat

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try 
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" 

