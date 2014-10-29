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

let rec contains i = function
  | [] -> false
  | hd :: tl -> if i = hd then true else contains i tl;;

let rec map f = function
  | [] -> []
  | hd :: tl -> (f hd) :: (map f tl);;
