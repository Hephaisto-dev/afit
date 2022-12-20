(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec pow_rec n =
    if n = [] then
      [0;1]
    else
      mult_b x (pow_rec (diff_b n [0;1]))
    in pow_rec n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n = 
  let rec power_rec x = function
     [] -> [0;1]
    | n when mod_b n [0;0;1] = [] -> power_rec (mult_b x x) (quot_b n [0;0;1])
    | n -> mult_b x (power_rec (mult_b x x) (quot_b n [0;0;1]))
  in power_rec x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m = 
  let rec mod_rec x r = function
    | [] -> r
    | e::l when e = 1 -> mod_rec (mod_b (mult_b x x) m) (mod_b (mult_b r x) m) l
    | e::l -> mod_rec (mod_b (mult_b x x) m) r l
  in match n with
       [] -> [0;1]
     | e::l -> mod_rec x [0;1] l;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = 
  if mod_b x p <> [] then
    let r = mod_b n (diff_b p [0;1]) in
    mod_power x r p
  else
    mod_power x n p;;
