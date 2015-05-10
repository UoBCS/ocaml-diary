(* Utils *)
let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  List.map Char.escaped (expl (String.length s - 1) []);;

let rec last = function
  | [] -> failwith "last"
  | x :: [] -> x
  | _ :: xs -> last xs
;;

let iexp n m = int_of_float (float_of_int n ** float_of_int m);;

let rec range a b =
  let rec aux acc a b =
    if a = b then acc
    else aux (acc @ [a]) (a + 1) b
  in aux [] a b
;;

let max cmp l =
  let rec aux mx = function
    | [] -> mx
    | x :: xs -> if cmp x mx > 0 then aux x xs  else aux mx xs
  in aux min_int l
;;

let max_tuple l =
  let rec aux mx = function
    | [] -> mx
    | (x1, x2) :: xs -> let (y1, y2) = mx in
			if x1 >= y1 && x2 >= y2 then aux (x1, x2) xs else aux mx xs
  in aux (min_int, min_int) l
;;

let is_consecutive l =
  let rec aux i = function
    | [] -> true
    | x :: xs -> x = i && aux (i + 1) xs
  in aux 1 l
;;

(* Multiples of 3 and 5 *)
let mul3_5 =
  let rec aux i =
    if i < 1000 then
      if i mod 3 = 0 || i mod 5 = 0 then i :: (aux (i + 1)) else aux (i + 1)
    else []
  in List.fold_left (+) 0 (aux 3)
;;

let prime_factors n =
  let rec aux ntry pfs = function
    | 1 -> pfs
    | m -> if m mod ntry = 0 then aux ntry (pfs @ [ntry]) (m / ntry) else aux (ntry + 1) pfs m
  in aux 2 [] n
;;

let largest_prime_factor n = max compare (prime_factors n);;

let largest_palindrome3 () =
  let rec aux acc = function
    | 1000, 1000 -> acc
    | n, m -> if m = 1000 then aux acc (n + 1, 100)
	      else
		let result = explode (string_of_int (n * m)) in
		if result = List.rev result then aux (acc @ [m, n]) (n, m + 1)
		else aux acc (n, m + 1)

  in last (aux [] (100, 100))
;;

let is_prime n =
  if n < 2 then false
  else if n = 2 then true
  else
    let rec aux = function
      | [] -> true
      | x :: xs -> if n mod x = 0 then false else aux xs
    in aux (range 2 n)
;;

let sum_primes n =
  let rec aux acc = function
    | 1 -> acc
    | n -> if is_prime n then aux (n + acc) (n - 1) else aux acc (n - 1)
  in aux 0 n
;;

let collatz_sequence n =
  let rec aux acc n =
    if n = 1 then acc
    else if n mod 2 = 0 then aux (acc @ [n / 2]) (n / 2)
    else aux (acc @ [3 * n + 1]) (3 * n + 1)
  in aux [n] n
;;

let power_digit_sum n exp =
  (iexp n exp) |> string_of_int |> explode |> List.map int_of_string |> List.fold_left (+) 0;;

let proper_divisors n =
  let rec aux i acc =
    if i = n then acc
    else if n mod i = 0 then aux (i + 1) (acc @ [i])
    else aux (i + 1) acc
  in aux 1 []
;;

let is_perfect n = (List.fold_left (+) 0 (proper_divisors n)) = n

let rec fact x = if x <= 1 then 1 else x * fact (x - 1);;

let digit_factorials n =
  n |> string_of_int |> explode |> List.map int_of_string |> List.map fact |> List.fold_left (+) 0;;

let is_pandigital_prime n =
  is_prime n &&
  (n |> string_of_int |> explode |> List.sort compare |> List.map int_of_string |> is_consecutive);;
