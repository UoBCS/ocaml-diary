type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec count = function
  | Empty                 -> 0
  | Node (_, left, right) ->
     let left_nodes = count left in
     let right_nodes = count right in
     1 + left_nodes + right_nodes;;

let rec sum = function
  | Empty                 -> 0
  | Node (x, left, right) -> x + (sum left) + (sum right);;

let rec preorder = function
  | Empty                 -> []
  | Node (x, left, right) -> x :: ((preorder left) @ (preorder right));;

let rec inorder = function
  | Empty                 -> []
  | Node (x, left, right) -> (inorder left) @ [x] @ (inorder right);;

let rec postorder = function
  | Empty                 -> []
  | Node (x, left, right) -> (postorder left) @ (postorder right) @ [x];;

let rec depth = function
  | Empty                 -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right);;

let rec insert k v = function
  | Empty -> Node ((k, v), Empty, Empty)
  | Node ((k', v'), left, right) ->
     if k = k' then failwith "Duplicate keys"
     else if k < k' then Node ((k', v'), insert k v left, right)
     else Node ((k', v'), left, insert k v right);;

(* Deletion *)
let rec first_inorder = function
  | Empty              -> failwith "Empty tree"
  | Node (a, Empty, _) -> a
  | Node (_, l, _)     -> first_inorder l;;

let rec delete k = function
  | Empty                -> Empty
  | Node ((k', v), l, r) ->
     if k < k'      then delete k Node ((k', v), delete k l, r)
     else if k > k' then delete k Node ((k', v), l, delete k r)
     else if l = Empty then r
     else if r = Empty then l
     else let (k'', v'') = first_inorder r in
	  Node ((k'', v''), l, delete k'' r)
;;

(* Check expression *)
type exp =
  | Val of int
  | Var of string
  | Sum of exp * exp
  | Prod of exp * exp
  | Let of string * exp * exp;;

let check_exp expr =
  let rec aux vars = function
    | Val x         -> true
    | Var x         -> List.mem vars x (* Check this *)
    | Sum (x, y)
    | Prod (x, y)   -> aux vars x && aux vars y
    | Let (n, v, e) ->
       if List.mem vars n
       then false
       else
	 aux vars v && aux (n :: vars) e
  in aux [] expr
;;
