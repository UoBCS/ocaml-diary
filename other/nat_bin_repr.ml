(* Implement a function add_bin_list that adds a list of binary numbers. 

add_bin_list : bool list list -> bool lis

For examples:

add_bin_list [ [true; true; false; true]; [false; false; true]; [true; false; true]] = [false; false; true; false; true; false; false]*)

let remove_zeros n =
  let rec aux = function
    | [] -> []
    | x :: xs -> if x = true then List.rev (x :: xs) else aux xs
  in aux (List.rev n)
;;

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

(* Write a function mul_bin_pow n k which multiplies a binary number n by the number k-th power of 2. 

mul_bin_pow : bool list -> int -> bool list

For example: 

mul_bin_pow [true; true; false; true] 3 = [false; false; false; true; true; false; true] *)

let rec fill value = function
  | 0 -> []
  | n -> value :: (fill value (n - 1));;

let mul_bin_pow n k = (fill false k) @ n;;

(*
Implement the function mul_bin which multiplies two binary numbers. 

mul_bin : bool list -> bool list -> bool list

For example:

mul_bin [false; true; true] [true; false; true] = [false; true; true; true; true]
 *)

let half = function
  | []      -> []
  | x :: xs -> xs
;;

let is_even = function
  | []      -> true
  | x :: xs -> not x
;;

let mul_bin n1 n2 =
  let rec aux acc = function
    | x, x' ->
       let l = half x in
       let r = mul_bin_pow x' 1 in
       if l = []
       then remove_zeros (add_bin_list acc)
       else if not (is_even l) then aux (acc @ [r]) (l, r) else aux acc (l, r)
  in aux [] (n1, n2)
;;
