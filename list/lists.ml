(*
    Functions operating on lists
    Concepts: lists, recursion
*)

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

let rec (===) = function
	| [], [] 							-> true
	| [], _::_ | _::_, [] -> false
	| x::xs, y::ys -> x = y && (xs === ys)
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
let rec unzip = function 
  | [] -> [], []
  | [x, y] -> [x], [y]
  | (x, y) :: tl -> let (a, b) = unzip tl in x :: a, y :: b;;

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

let rec prelist a b = match a, b with
	| [], _ -> true
	| _, [] -> false
	| x :: xs, y :: ys -> x = y && prelist xs ys;;

let rec sublist a b = match a, b with
	| [], _ -> true
	| _, [] -> false
	| x :: _, y :: ys -> (prelist a b) || (sublist a ys);;
	
(* Merge 2 sorted lists *)
let rec mergels l1 = function
	| [] -> l1
	| x :: xs -> mergels (insert x l1) xs;;

(* Encode *)
let rec precount e = function
	| [] -> 0
	| x :: xs -> if x = e then 1 + precount e xs else 0
;;

let encode l =
	let rec encode' c = function
		| [] -> []
		| x :: xs -> if precount x (x :: xs) = 1
						then (x, c) :: (encode' 1 xs)
						else encode' (c + 1) xs
	in encode' 1 l
;;

(* Split on index *)
let splitn n l =
	let rec splitn' acc = function
		| _, [] 	 -> ([], [])
		| 0, s 		 -> (acc, s)
		| n, x :: xs -> splitn' (acc @ [x]) ((n - 1), xs)
	in splitn' [] (n, l)
;;

(* Slice *)
let slicem lo up l =
	let rec slicem' c = function
		| [] 		-> []
		| x :: xs   ->
			if c > up then []
			else if c >= lo
			then x :: slicem' (c + 1) xs
			else slicem' (c + 1) xs
	in slicem' 0 l
;;

(* Remove at index *)
let remove_at l i =
	let rec aux c = function
		| [] 	  -> []
		| x :: xs ->
			if      c < i then x :: aux (c + 1) xs
			else if c > i then (x::xs)
			else          aux (c + 1) xs
	in aux 0 l
;;

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

let rec repeat s = function
| 0 -> []
| n -> s :: repeat s (n - 1);;

let rec replicate l n = match (l, n) with
| [], _      -> []
| l, 0       -> l
| x :: xs, n -> (repeat x n) @ replicate xs n;;

let rec split = function
    | []        -> [], []
    | x :: []   -> [x], []
    | x :: x' :: xs   -> let left, right = split xs in x :: left, x' :: right;;

(* Selection sort *)
let remove x = function
	| [] -> []
	| y :: ys -> if x = y then ys else y :: (remove x ys)
;;

let selection_sort = function
	| [] -> []
	| [x] -> [x]
	| xs -> let x = minl xs in (* Get minimum *)
			let xs' = remove x xs in
			x :: (selection_sort xs')
;;		 

(* Insertion sort *)
let rec insert x = function
    | []      -> [x]
    | y :: ys -> if x < y then (x :: y :: xs) else (y :: insert x ys);;

let rec insert_sort = function
    | []      -> []
    | x :: xs -> insert x insert_sort xs;;

(* Quicksort *)
let rec quick_sort = function
    | []      -> []
    | x :: xs ->
            let smaller, greater = partition (fun a -> a < x) xs in
            (quick_sort smaller) @ [x] @ (quick_sort greater);;

(* Merge sort *)
let rec merge = function
    | xs, []           -> xs
    | [], ys           -> ys
    | x :: xs, y :: ys ->
            if x < y
            then x :: merge (xs, y :: ys)
            else y :: merge (x :: xs, ys)

let rec merge_sort = function
    | []  -> []
    | [x] -> [x]
    | xs  ->
            let xs', xs'' = split xs in
            let xs'       = merge_sort xs'' in
            let xs''      = merge_sort xs'' in
            merge (xs', xs'')
;;

 let rec insert_at x i = function
     | []       -> [x]
     | hd :: tl ->
             if i = 0
             then x :: hd :: tl
             else hd :: insert_at x (i - 1) tl
 ;;

let rec insert_at' e i l = match (i, l) with
	| _, [] 	 -> []
	| 0, s 	 	 -> e :: s
	| n, x :: xs -> x :: insert_at' e (i - 1) xs;;


let rec rem_leading x = function
     | []      -> []
     | y :: ys -> if y = x then rem_leading ys else y :: ys
 ;;
 
let rec fill x = function
| 0 -> []
| n -> let m = fill x (n - 1) in x :: m;;

(* This function takes the first n elements of a list *)
let rec take n xs = match (n, xs) with
  | 0, _       -> []
  | n, x :: xs -> let m = take (n - 1) xs in x :: m;;

(* This function removes the first n elements of a list *)
let rec drop n xs = match (n, xs) with
  | 0, _       -> xs
  | n, []      -> []
  | n, x :: xs -> 
     if n <> 0 
     then drop (n - 1) xs
     else xs
;;

(* The above function using fold right *)
let dropf l n = fst (
	fold_right (fun (a, c) x ->
		if c mod n = 0
		then (a, c + 1)
		else (a @ [x], c + 1))
	([], 1) l
);;

(* This function 'rotates' the first n elements from the head to the tail *)
let rec rotate n xs = match (n, xs) with
  | 0, _       -> xs
  | n, []      -> []
  | n, x :: xs -> rotate (n - 1) xs @ [x]
;;

let every n xs =
  let rec aux n xs i = match (n, xs) with
    | 1, _       -> xs
    | n, []      -> []
    | n, y :: ys ->
       if i mod n = 0
       then y :: (aux n ys (i + 1))
       else aux n ys (i + 1)
  in aux n xs 1
;;

let rec interleaven = function
  | []      -> []
  | x :: xs ->
     match x with
     | []      -> interleaven xs
     | y :: ys -> y :: interleaven (xs @ [ys])
;;

(* Find connected components *)
let rec elim_consec = function 
  | [] -> []
  | [x] -> [x]
  | x :: x' :: xs -> 
	if x = x' then elim_consec (x' :: xs) 
	else x :: elim_consec (x' :: xs)
;;

let elim_dupls xs = elim_consec (List.sort compare xs);;  (* xs |> sort compare |> elim_consec ;; *)

let combine xs ys =
	elim_dupls (List.rev_append xs ys);;

let rec expand x graph explored =
  match graph with 
	| [] -> []
	| (y, y') :: ys ->
	  if x <> y && x <> y' 
	  then expand x ys explored
	  else let x' = if x = y then y' else y in 
		   if List.mem explored x'
		   then expand x ys explored
		   else x' :: (expand x ys  explored) 
;;

let graph_search graph node =
  let rec aux graph frontier explored =
    match frontier with
    | []      -> explored
    | x :: xs ->
       let neighbours = expand x graph explored in
       let new_frontier = combine xs neighbours in
       aux graph new_frontier (explored @ [x])
  in aux graph [node] []
;;

(* With flatten *)
let components graph = 
  let rec aux fin = function
    | []      -> []
    | x :: xs ->
       if List.mem fin x 
       then aux fin xs
       else let comp = List.sort compare (graph_search graph x) in
	    comp :: (aux (fin @ comp) xs)
  in aux [] (graph |> flatten |> elim_dupls)
;;
