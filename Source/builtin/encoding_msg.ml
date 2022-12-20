(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let multiplier = power 2 bits  in
  let rec fold m = function
      n when n = String.length str -> m
    | n -> fold (m*multiplier + Char.code str.[n]) (n+1)
  in fold 0 0;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let multiplier = power 2 bits in
  let rec fold = function
      0 -> ""
    | n -> fold (n/multiplier) ^ Char.escaped (Char.chr (n mod multiplier))
  in fold msg;;
