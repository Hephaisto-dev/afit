(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec pow_rec n =
    if n = 0 then
      1
    else
      x * pow_rec (n-1)
    in pow_rec n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec power_rec x = function
      0 -> 1
    | n when n mod 2 = 0 -> power_rec (x*x) (n/2)
    | n -> x * power_rec (x*x) (n/2)
  in power_rec x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
(* mod is used when a mod b > 0 with OCAML default, so when a > 0 *)
let mod_power x n m =
  let rec mod_rec x r = function
    | 0 -> r
    | n when n mod 2 = 1 -> mod_rec (x*x mod m) (modulo (r*x) m) (n/2)
    | n -> mod_rec (x*x mod m) r (n/2)
  in mod_rec (modulo x m) 1 n;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
(* x^n = x^(q*(p-1)+r) = (x^(p-1))^q*x^r = x^r mod p *)
let prime_mod_power x n p =
  if x mod p <> 0 then
    let r = modulo n (p-1) in
    mod_power x r p
  else
    mod_power x n p;;
