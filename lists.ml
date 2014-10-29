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

(* some lists we will use in examples *)
let l  = [0; 1; 2; 3; 4];;
let l' = [5; 6; 7; 8; 9];;

(* 1. calculate the sum of all elements of a list *)
let rec sum = function
  | []      -> 0
  | x :: xs -> x + sum xs
;;

sum l = 10;;

(* 2. calculate the smallest element of a list *)
let rec minl = function
  | []		 -> failwith "minl"
  | [x] 	 -> x
  | x1 :: xs -> let x2 = minl xs in if x1 < x2 then x1 else x2
;;

minl l = 0;;

(* 3. 'zip' two lists of equal length into a list of pairs *)
let rec zip = function
  | [], []                  -> []
  | [x], [y]                -> [(x, y)]
  | hd1 :: tl1, hd2 :: tl2  -> (hd1, hd2) :: zip (tl1, tl2)
  | _ :: _, [] | [], _ :: _ -> failwith "zip"
;;

zip (l, l') = [(0, 5); (1, 6); (2, 7); (3, 8); (4, 9)];;

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

(l, l') |> zip |> flatten = [0; 5; 1; 6; 2; 7; 3; 8; 4; 9];;
