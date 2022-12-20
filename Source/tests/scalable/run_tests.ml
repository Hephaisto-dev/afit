open Alcotest
open Test_tools
open Test_scalable
open Test_scalable_basic_arithmetics
open Test_scalable_power
open Test_scalable_test_primes
open Test_scalable_generate_primes
open Test_scalable_encoding_msg
open Test_scalable_ciphers
open Test_scalable_break_ciphers

let scalable_test_suite =
    [("scalable_basic_arithmetics",  basic_arithmetics_set);
     ("scalable_power",              power_set);
     ("scalable_test_primes",        test_primes_set)]

let () = run_to_xml "trace_scalable_1.xml" [scalable_test_suite]
