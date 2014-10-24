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
