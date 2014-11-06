(* Factorial function  *)
let rec fact n = match n with
  | 0 -> 1
  | _ -> n * (fact (n - 1))
;;

(* Cumulative sum function *)
let rec cumulative_sum n = match n with
  | 1 -> 1
  | _ -> n + cumulative_sum (n - 1)
;;

let is_upper c = match c with
  | 'A'..'Z' -> true
  | _ -> false
;;

let is_lower c = not (is_upper c);;

let is_bij f = f true <> f false;;
