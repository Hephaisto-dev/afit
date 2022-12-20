(** Test suites for builtin basic_arithmetic ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable
open Scalable_basic_arithmetics

let sprintf = Printf.sprintf

let gcd_b_tests () =
    let cases =
        [(from_int 32, from_int 6), from_int 2;
         (from_int 18, from_int 12), from_int 6;
         (from_int (-18), from_int (-12)), from_int 6]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "gcd_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (gcd_b a b)
    in
    List.iter do_check cases

let bezout_b_tests () =
    let cases =
        [(from_int 18, from_int 22), (from_int 5, from_int (-4), from_int 2);
         (from_int 22, from_int 18), (from_int (-4), from_int 5, from_int 2);
         (from_int 17, from_int 21), (from_int 5, from_int (-4), from_int 1);
         (from_int 21, from_int 17), (from_int (-4), from_int 5, from_int 1)]
    and do_check ((a, b), expected) =
        check
            (triplet (list int) (list int) (list int))
            (sprintf "bezout_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (bezout_b a b)
    in
    List.iter do_check cases


(****************************************************************************)
(****************************************************************************)

let basic_arithmetics_set =
    [("GCD on bitarrays function", `Quick, gcd_b_tests);
     ("Bezout on bitarrays function", `Quick, bezout_b_tests)]
