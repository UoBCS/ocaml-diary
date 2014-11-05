let empty = function
  | []     -> true
  | _ :: _ -> false

let hd = function 
  | []      -> failwith "hd"
  | x :: xs -> x
;;

let tl = function 
  | []      -> failwith "tl"
  | x :: xs -> xs
;;

let rec nth = function 
  | 0, x :: _  -> x
  | n, _ :: xs -> nth (n - 1, xs)
  | _, _       -> failwith "nth"
;;

let rec append xs ys = match xs with
  | []      -> ys 
  | x :: xs -> x :: (append xs ys) 
;;

let rec rev = function 
  | []      -> []
  | (x:int) :: xs -> append (rev xs) [x]
;;

(* 1. calculate the sum of all elements of a list *)
let rec sum = function
  | []      -> 0
  | x :: xs -> x + sum xs
;;

(* 2. calculate the smallest element of a list *)
let rec minl = function
  | []		 -> failwith "minl"
  | [x] 	 -> x
  | x1 :: xs -> let x2 = minl xs in if x1 < x2 then x1 else x2
;;

(* 3. 'zip' two lists of equal length into a list of pairs *)
let rec zip = function
  | [], []                  -> []
  | [x], [y]                -> [(x, y)]
  | hd1 :: tl1, hd2 :: tl2  -> (hd1, hd2) :: zip (tl1, tl2)
  | _ :: _, [] | [], _ :: _ -> failwith "zip"
;;

(* 4. 'unzip' a list of pairs into two lists *)
(* let rec unzip = function
  | [] -> ([], [])
  | [(x, y)] -> ([x], [y])
  | (x, y) :: tl -> x :: unzip tl, y :: unzip tl
;;

(l, l') |> zip |> unzip = (l, l');;
*)

(* 5. 'flatten' a list of pairs into a single list *)
let rec flatten = function
  | [] -> []
  | (x1, x2) :: xs -> x1 :: x2 :: flatten xs;;

let rec range a b =
  if a = b then [a]
  else a :: range (a + 1) b;;

(* Membership check *)
let rec mem i = function
  | []       -> false
  | hd :: tl -> i = hd || mem i tl;;

(* Check for property satisfaction (predicate logic) *)
let rec find p = function
  | []       -> false
  | hd :: tl -> if p hd then hd else find p tl;;

let rec filter p = function
  | []      -> []
  | x :: xs -> if p x then x :: filter p xs else filter p xs;;

let rec partition p = function
  | [] -> [], []
  | x :: xs -> let (ps, nps) = partition p xs in
	       if p x then (x :: ps, nps) else (ps, x :: nps);;

let rec map f = function
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl);;

let rec remove_odds = function
  | [] -> []
  | hd :: tl -> if hd mod 2 = 0 then hd :: (remove_odds tl) else remove_odds tl;;

let rec add = function
  | [], [] -> []
  | xs, [] | [], xs -> xs
  | hd1 :: tl1, hd2 :: tl2 -> (hd1 + hd2) :: (add (tl1, tl2))

let rec interleave = function
  | [], []                 -> []
  | xs, [] | [], xs        -> xs
  | hd1 :: tl1, hd2 :: tl2 -> hd1 :: hd2 :: (interleave (tl1, tl2));;

let rec sort = failwith "TODO: sort";;

(* Repeat 'x' y times *)
let rec repeat x y = match y with
  | 0 -> []
  | a -> x :: repeat x (a - 1);;

let rec assoc a = function
  | [] -> failwith "assoc"
  | (k, v) :: xs -> if a = k then v else assoc a xs;;

let rec prefix xs ys = match (xs, ys) with
| [], _::_ | [], [] -> true
| _::_, [] -> false
| x1 :: xs, y1 :: ys -> x1 = y1 && prefix xs ys;;

(* Remove last element in list *)
let rem_last xs = match rev xs with
  | [] -> []
  | x :: xs -> rev xs;;

(* Get prefixes of the given list *)
let rec prefixes xs = match xs with
    | []      -> [[]]
    | l       -> prefixes (rem_last l) @ [l]
;;

let rec last = function
| [] -> failwith "last"
| x :: xs -> if xs = [] then x else last xs;;

let is_palindrome l =
	l = rev l;;


