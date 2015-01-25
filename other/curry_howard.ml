(*
Logic        <----> Computation
Propositions <----> Types
Proofs       <----> Programs
Provability  <----> Inhabitation
*)

fun x -> x;; (* a -> a Identity *)

fun x f -> f x;; (* a -> (a -> b) -> b Implication elimination (modus ponens) *)

fun f g x -> f x (g (x));;  (* (a -> b -> c) -> (a -> b) -> (a -> c) *)

fun (x, y) -> x;;  (* (a and b) -> a Conjunction elimination using projection *)

(* Disjunction *)
type ('a, 'b) dis = Left of 'a | Right of 'b;;

fun x -> Left x;;  (* a -> (a v b) Disjunction introduction *)

(* a v b -> (a -> c) -> (b -> c) -> c Disjunction elimination *)
fun a_or_b f g ->
      match a_or_b with
      | Left x  -> f x
      | Right x -> g x
;;
		 
(* Negation *)
type bot;; (* Type bottom --> contradiction *)

(* De Morgan Laws (not A and not B) -> not (A v B) same as (A -> bot and B -> bot) -> (A v B) -> bot  *)
(* (a -> bot, b -> bot) -> (a, b) dis -> bot *)
fun (f, g) a_or_b -> match a_or_b with
		     | Left x  -> f x
		     | Right x -> g x
;; 

(fun f x -> f (Left x), fun f x -> f (Right x));;
