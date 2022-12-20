(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = 
  let (n,_) = key in
  if mod_b n [0;0;1] = [] then
    ([0;0;1],quot_b n [0;0;1])
  else
    let rec brute_force k =
      if mod_b n k = [] then
        (k,quot_b n k)
      else
        brute_force (diff_b k [0;0;1])
    in brute_force n;;
