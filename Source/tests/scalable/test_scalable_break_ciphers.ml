(** Test suites for scalable break_cifers ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable
open Scalable_break_ciphers

let sprintf = Printf.sprintf

(* Only tests for RSA for now. *)

let break_tests () =
    let cases = [((from_int 99400891, from_int 36199003), (from_int 9967, from_int 9973))]
    and do_check ((n, e), expected) =
        check
            (pair (list int) (list int))
            (sprintf "break: n=%s and e=%s" (string_of_intlist n) (string_of_intlist e))
            expected
            (break (n, e))
    in
    List.iter do_check cases

let break_ciphers_set =
    [("RSA key breaking with bitarrays function", `Quick, break_tests)]
