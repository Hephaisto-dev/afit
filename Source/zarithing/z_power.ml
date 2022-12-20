(** Power function implementations for big integers *)

open Z

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec mod_rec x r = function
    | n when n = zero -> r
    | n when erem n (of_int 2) = one -> mod_rec (erem (mul x x) m) (erem (mul r x) m) (shift_right n 1)
    | n -> mod_rec (erem (mul x x) m) r (shift_right n 1)
  in mod_rec (erem x m) one n;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if erem x p <> zero then
    let r = erem n (pred p) in
    mod_power x r p
  else
    mod_power x n p;;

