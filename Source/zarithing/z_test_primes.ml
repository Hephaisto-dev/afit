(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n =
  let rec prime_rec divisor =
    divisor * divisor > n || (* sqrt stop point, better optimisation *)
      (erem n divisor <> zero && prime_rec (add divisor (succ one))) (* complexity / 2 *)
  in n > one && (n = succ one || (erem n (succ one) <> zero && prime_rec (succ (succ one))));; (* only odd *)
