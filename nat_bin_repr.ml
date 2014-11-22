(* Implement a function add_bin_list that adds a list of binary numbers. 

add_bin_list : bool list list -> bool lis

For examples:

add_bin_list [ [true; true; false; true]; [false; false; true]; [true; false; true]] = [false; false; true; false; true; false; false]*)

let rec int_to_bin = function 
  | 0 -> [false]
  | n -> not (n mod 2 = 0) :: (int_to_bin (n / 2));;

let add_3 b1 b2 b3 = match b1, b2, b3 with 
  | true, true, true -> true, true
  | true, true, false
  | true, false, true
  | false, true, true -> true, false
  | true, false, false
  | false, true, false
  | false, false, true -> false, true
  | false, false, false -> false, false;;

let rec add_bin_aux n1 n2 carry = match n1, n2 with 
  | [], [] -> [carry]
  | [], n2 -> add_bin_aux [false] n2 carry
  | n1, [] -> add_bin_aux n1 [false] carry
  | d1 :: d1s, d2 :: d2s ->
	let (carry, d) = add_3 d1 d2 carry in
	d :: (add_bin_aux d1s d2s carry);;

let add_bin n1 n2 = add_bin_aux n1 n2 false;;

let rec add_bin_list = function
  | []             -> [false]
  | x :: []        -> x
  | [x1; x2]       -> add_bin x1 x2
  | x1 :: x2 :: xs -> add_bin (add_bin x1 x2) (add_bin_list xs);;
