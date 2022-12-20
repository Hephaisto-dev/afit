(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
   
let crt_image x l =
  let rec fold = function
      [] -> []
    | e::l -> x mod e :: fold l
  in fold l;;

(** Inverse image of Chinese Remainder map
    @param m a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y =
  let rec solve = function
      ([],[]) -> 0
    | (e1::l1,e2::l2) -> let ni = m/e1 in
                         let (invert,_,_) = bezout ni e1
                         in e2*ni*invert + solve (l1,l2)
    | _ -> failwith "Not the same size of lists"
  in solve (l,y) mod m;;
