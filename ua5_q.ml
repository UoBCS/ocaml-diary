(* This function takes the first n elements of a list *)

let rec take n xs = match (n, xs) with
  | 0, _       -> []
  | n, x :: xs -> let m = take (n - 1) xs in x :: m;;

take 2 [1;2;3;4;5] = [1; 2];;


(* This function removes the first n elements of a list *)

let rec drop n xs = ?

drop 3 [1;2;3;4;5] = [4; 5];;

(* This function 'rotates' the first n elements from the head to the tail *)

let rec rotate n xs = ?

rotate 3 [1;2;3;4;5] = [4; 5; 1; 2; 3];;

(* This function drops n elements from the tail *)
let rec drop' n xs = ?

drop' 3 [1;2;3;4;5] = [1; 2];;

(* This function takes n elements from the tail *)
let rec take' n xs = ?

take' 3 [1;2;3;4;5] = [3; 4; 5];;

(* This function rotates n elements from the tail to the head *)
let rec rotate' n xs = ?

rotate' 2 [1; 2; 3; 4; 5] = [4; 5; 1; 2; 3];;
