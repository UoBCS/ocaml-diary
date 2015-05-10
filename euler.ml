(* Multiples of 3 and 5 *)
let mul3_5 =
  let rec aux i =
    if i < 1000 then
      if i mod 3 = 0 || i mod 5 = 0 then i :: (aux (i + 1)) else aux (i + 1)
    else []
  in List.fold_left (+) 0 (aux 3)
;;

