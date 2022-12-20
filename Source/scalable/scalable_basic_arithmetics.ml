(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB = 
  if bB = [] then [] else
    let rec gcd bA bB =
      let r = mod_b bA bB in
      if r = [] then
        bB
      else
        gcd bB r
    in gcd bA bB;;

(** Extended euclidean division of tcompare_b wo integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  if bA = [] || bB = [] then
    invalid_arg "bezout: Arguments must be <> []"
  else
    let rec diophantine a b u1 v1 u2 v2 =
      let r = mod_b a b in
      if r = [] then
        (u2,v2,b)
      else
        let q = diff_b [] (quot_b a b) in (* r = a + b * q *)
        diophantine b r u2 v2 (add_b u1 (mult_b u2 q)) (add_b v1 (mult_b v2 q)) (* Direct method to
                                                             find bezout
                                                             coefficient *)
    in diophantine bA bB [0;1] [] [] [0;1];;
