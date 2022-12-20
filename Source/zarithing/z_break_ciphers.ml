(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,_) = key in
  if erem n (succ one) = zero then
    (succ one,n/(succ one))
  else
    let rec brute_force k =
      if erem n k = zero then
        (k,n/k)
      else
        brute_force (k-(succ one))
    in brute_force (succ (sqrt n)/succ(one)*succ(one)+one);;
