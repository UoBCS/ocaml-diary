open List;;

let head = function
    | []      -> failwith "head"
    | x :: _ -> x
;;

let tail = function
    | []      -> failwith "tail"
    | _ :: xs -> xs
;;

let enq (x, (f, r)) = (f, x :: r);;

let deq = function
    | f, x :: r  -> (x, (f, r))
    | [], r      -> 
            let f = rev r in
            (head f, (tail f, r))
;;

let map_q g (f, r) = (List.map g f, List.map g r);;

let swap f a b = f b a;;

let fold_q g a (f, r) =
    List.fold_right (swap f) r (List.fold_left g a f)
