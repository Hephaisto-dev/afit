(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let multiplier = shift_left one bits in
  let rec fold m = function
      n when n = of_int (String.length str) -> m
    | n -> fold (add (mul m multiplier)  (of_int (Char.code str.[to_int n]))) (add n one)
  in fold zero zero;;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let multiplier = shift_left one bits in
  let rec fold = function
      n when n = zero -> ""
    | n -> fold (ediv n multiplier) ^ Char.escaped (Char.chr (to_int (erem n multiplier)))
  in fold msg;;

