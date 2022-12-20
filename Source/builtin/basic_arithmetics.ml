(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
    Theoretical complexity: 2log2 (b) + 2
 *)

let rec gcd a b =
  let r = modulo a b in
  if r = 0 then
    b
  else
    gcd b r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
 *)
let bezout a b = (* Theory is visible here: https://youtu.be/Ev_BmUwji_w *)
  if a = 0 || b = 0 then
    invalid_arg "bezout: Arguments must be <> 0"
  else
    let rec diophantine a b u1 v1 u2 v2 =
      let r = modulo a b in
      if r = 0 then
        (u2,v2,b)
      else
        let q = - (quot a b) in (* r = a + b * q *)
        diophantine b r u2 v2 (u1 + u2 * q) (v1 + v2 * q) (* Direct method to
                                                             find bezout
                                                             coefficient *)
    in diophantine a b 1 0 0 1;;
