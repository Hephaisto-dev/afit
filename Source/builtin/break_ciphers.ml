(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,_) = key in
  if n mod 2 = 0 then
    (2,n/2)
  else
    let rec brute_force k =
      if n mod k = 0 then
        (k,n/k)
      else
        brute_force (k-2)
    in brute_force ((int_of_float (sqrt (float_of_int (n))) + 1)/2*2+1)
