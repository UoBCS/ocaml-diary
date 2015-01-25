(* Queues *)
type 'a naive_queue = 'a list;;

let enq a q = q @ [a];;

let deq q = (List.hd q, List.tl q);;

let is_empty q = (q = []);;

let empty_q () = [];;

let map_q = List.map;;

let fold_q = List.fold_left;;
