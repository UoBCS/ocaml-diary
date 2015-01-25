(*
     Basic OCaml exercises involving numbers and strings 
*)

(* Factorial function *)
let rec fact n = match n with
  | 0 -> 1
  | _ -> n * (fact (n - 1))
;;

(* Cumulative sum function *)
let rec cumulative_sum n = match n with
  | 1 -> 1
  | _ -> n + cumulative_sum (n - 1)
;;

(* Checks whether a character is upper-case *)
let is_upper = function
  | 'A'..'Z' -> true
  | _ -> false
;;

(* Checks whether a character is lower-case *)
let is_lower c = not (is_upper c);;

(* Is function f bijective? *)
let is_bij f = f true <> f false;;
