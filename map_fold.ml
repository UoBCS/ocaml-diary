(* High-order functions on lists and trees *)

(* Map function (non tail recursive approach *)
let rec map f = function
  | []      -> []
  | x :: xs -> (f x) :: (map f xs);;

(* Map function (tail recursive approach 1: reverses the list) *)
let rec map f acc = function
  | []      -> acc
  | x :: xs -> map f ((f x) :: acc) xs;;

(* Map function (tail recursive 2: keeps the order *)
let rec _map f acc = function
  | []      -> List.rev acc
  | x :: xs -> _map f ((f x) :: acc) xs;;

let rec map f l = _map f [] l;;

(* Fusion --> use map multiple times: composition operator *)
let (%) f g = fun x -> f (g x);;

(* Fold function *)
let rec fold f a = function
  | []      -> a
  | x :: xs -> f x (fold f a xs);;

(* Fold function (tail recursive approach) *)
let rec fold f a = function
  | []      -> a
  | x :: xs -> fold f (f x a) xs;;

(* Usage of fold *)
let sum = fold (+) 0;;

let len = fold (fun _ a -> 1 + a) 0;;

(* Average function (without recursion) *)
let avg 
