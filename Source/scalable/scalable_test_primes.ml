(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n = 
  let rec prime_rec divisor =
    mult_b divisor divisor >> n || (* sqrt stop point, better optimisation *)
      (mod_b n divisor <> [] && prime_rec (add_b divisor [0;0;1])) (* complexity / 2 *)
  in n >> [0;1] && (n = [0;0;1] || (mod_b n [0;0;1] <> [] && prime_rec [0;1;1])) ;; (* only odd *)

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec pseudo = function
      [] -> true
    | e::l -> mod_power e p p = mod_b e p && pseudo l
  in pseudo test_seq;;
