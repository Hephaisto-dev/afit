(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec prime_rec divisor =
    divisor * divisor > n || (* sqrt stop point, better optimisation *)
      (n mod divisor <> 0 && prime_rec (divisor + 2)) (* complexity / 2 *)
  in n > 1 && (n = 2 || (n mod 2 <> 0 && prime_rec 3)) ;; (* only odd *)

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec pseudo = function
      [] -> true
    | e::l -> mod_power e p p = modulo e p && pseudo l
  in pseudo test_seq;;
