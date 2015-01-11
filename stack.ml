type 'a stack = 'a list;;

let create () = [];;

let push el st = el :: st;;

let pop = function
  | []       -> failwith "pop"
  | e :: st  -> (e, st)
;;

let top = function
  | []     -> failwith "top"
  | e :: _ -> e
;;

let is_empty s = (s = []);;

let length l =
  let rec aux acc = function
    | []     -> acc
    | _ :: s -> aux (acc + 1) s
  in aux 0 l
;;

let rec map f = function
  | []      -> []
  | e :: st -> (f e) :: (map f st)
;;
