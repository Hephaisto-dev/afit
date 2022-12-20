(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits = 
  let multiplier = power [0;0;1] (from_int bits) in
  let rec fold m = function
      n when n = String.length str -> m
    | n -> fold (add_b (mult_b m multiplier) (from_int (Char.code str.[n]))) (n+1)
  in fold [] 0;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits = 
  let multiplier = power [0;0;1] (from_int bits) in
  let rec fold = function
      [] -> ""
    | n -> fold (quot_b n multiplier) ^ Char.escaped (Char.chr (to_int (mod_b n multiplier)))
  in fold msg;;
