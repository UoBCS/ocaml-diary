type 'a naive_queue

val empty_q : unit -> 'a naive_queue
val is_empty : 'a naive_queue -> bool
val enq : 'a -> 'a naive_queue -> 'a naive_queue
val deq : 'a naive_queue -> ('a * 'a naive_queue)
val map_q : ('a -> 'b) -> 'a naive_queue -> 'b naive_queue
val fold_q : ('a -> 'b -> 'a) -> 'a -> 'b naive_queue -> 'a
