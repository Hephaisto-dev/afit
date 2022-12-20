(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Remove useless 0 of a bitarray (used for difference) 
    @param bA the bitarray to simplify
*)
let simplify bA =
  let rec reverse buffer = function
      []   -> buffer
    | e::l -> reverse (e :: buffer) l
  in let rec simp = function
      [] -> []
    | e::l when e = 0 -> simp l
    | l -> l
  in reverse [] (simp (reverse [] bA));;

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  if x = 0 then [] else
  let rec create_array = function
      x  when x/2 = 0 -> [1]
    | x -> x mod 2 :: create_array (x/2)
  in 
  if x < 0 then
    1 :: create_array (-x)
  else 
    0 :: create_array x;;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA =
  let rec create_int = function
      [] -> 0
    | e::l -> (e + create_int l * 2)
  in
  match bA with
      [] -> 0
    | e::l when e=1 -> -1 * create_int l
    | e::l -> create_int l;;
       

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec print_next = function
      [] -> ()
    | e::l -> print_next l; print_int e
  in print_next bA;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)

let rec compare_n nA nB =
  let rec compare buffer = function
      [],[] -> buffer
    | [],l -> -1
    | l,[] -> 1
    | e1::l1, e2::l2 when e1 < e2 -> compare (-1) (l1,l2)
    | e1::l1, e2::l2 when e1 > e2 -> compare 1 (l1,l2)
    | e1::l1, e2::l2 -> compare buffer (l1,l2)
  in compare 0 (nA, nB);;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = compare_n nA nB = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = compare_n nA nB = -1;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = compare_n nA nB = 1 || compare_n nA nB = 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = compare_n nA nB = -1 || compare_n nA nB = 0;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = match (bA,bB) with
    [],[] -> 0
  | e::l,[] when e=1 -> -1 (* Pre-check for not calling compare_n *)
  | [],e::l when e=0 -> -1
  | l,[] | [],l -> 1 
  | e1::l1,e2::l2 when e1=e2 && e1=0 ->  compare_n l1 l2
  | e1::l1,e2::l2 when e1=e2 -> -1 * compare_n l1 l2
  | e1::l1,e2::l2 when e1<e2 -> 1
  | _ -> -1;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = compare_b bA bB = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = compare_b bA bB = -1;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = compare_b bA bB = 1 || compare_b bA bB = 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = compare_b bA bB = -1 || compare_b bA bB = 0;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
    [] -> 1
  | e::l when e=1 -> -1
  | _ -> 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
    [] -> []
  | e::l-> 0::l;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = 
  let rec add buffer = function
      [],[] when buffer = 1 -> [1]
    | [],[] -> [] 
    | [],e::l when e + buffer > 1 -> 0 :: add 1 ([],l) (* Similar reflexion than for compare *)
    | e::l,[] when e + buffer > 1 -> 0 :: add 1 (l,[])
    | [],e::l | e::l,[] -> buffer + e :: l
    | e1::l1,e2::l2 when e1 + e2 + buffer <= 1 -> e1 + e2 + buffer :: add 0 (l1,l2)
    | e1::l1,e2::l2 when e1 + e2 + buffer > 1 && e1 + e2 + buffer <> 2 -> 1 :: add 1 (l1,l2)
    | e1::l1,e2::l2 when e1 + e2 + buffer > 1 -> 0 :: add 1 (l1,l2)
    | _ -> []
  in add 0 (nA,nB);;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = 
  let rec diff buffer = function
      [],[] -> []
    | [],l -> invalid_arg "nA must be > nB"
    | e::l,[] when e - buffer >= 0 -> (e-buffer) :: diff 0 (l,[])
    | e::l,[] -> 1 :: diff buffer (l,[])
    | e1::l1,e2::l2 when e1 - e2 - buffer = -2 -> 0 :: diff 1 (l1,l2)
    | e1::l1,e2::l2 when e1 - e2 - buffer = -1 -> 1 :: diff 1 (l1,l2)
    | e1::l1,e2::l2 when e1 - e2 - buffer = 0 -> 0 :: diff 0 (l1,l2)
    | e1::l1,e2::l2 when e1 - e2 - buffer = 1 -> 1 :: diff 0 (l1,l2)
    | _ -> []
  in simplify (diff 0 (nA,nB));;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = match (bA,bB) with
    [],[] -> []
  | l,[] -> l
  | [],l -> l
  | e1::l1,e2::l2 when e1 = e2 -> e1 :: add_n l1 l2
  | e1::l1,e2::l2 when e1 = 1 && l1 >>! l2 -> 1 :: diff_n l1 l2
  | e1::l1,e2::l2 when e1 = 1 -> 0 :: diff_n l2 l1
  | e1::l1,e2::l2 when l1 <<! l2 -> 1 :: diff_n l2 l1
  | e1::l1,e2::l2 -> 0 :: diff_n l1 l2;;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  let body bA bB = match (bA,bB) with
    [],[] -> []
  | l,[] -> l
  | [],e::l -> (1-e)::l (* Change sign *)
  | _,e2::l2 -> add_b ((1 - e2) :: l2) bA (* if e2 = 0 => negative entry else if e2=1 positive entry *)
  in simplify (body bA bB);;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = match (bA,d) with
    l,0 -> l
  | [],d -> []
  | e::l,d -> shift (e::0::l) (d-1);;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = 
  let rec mult buffer = function
      [],[] -> []
    | e::l,[] | [],e::l -> []
    | l1,e2::l2 when e2 = 0 -> mult (buffer + 1) (l1,l2)
    | l1,e2::l2 -> add_b (shift l1 buffer) (mult (buffer + 1) (l1,l2))
  in match (bA,bB) with 
       e::l,[] | [],e::l -> []
     | e1::l1,e2::l2 when e1 = e2 -> mult 0 (0 :: l1,l2)
     | e1::l1,e2::l2 -> mult 0 (1 :: l1,l2)
     | _  -> [];;

(** Quotient of two naturals.
    @param nA Natural you want to divide by second argumen.
    @param nB natural you divide by. Non-zero!.
*)
let quot_n nA nB = 
  if nB = [] then invalid_arg "nB must be <> 0" else
  let rec quot buffer = function
      (l1,l2) when l1<<!l2 -> buffer
    | (l1,l2) -> quot (add_n buffer [0;1]) (diff_n l1 l2,l2) 
  in quot [] (nA,nB);;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = match (bA,bB) with
    _, [] -> invalid_arg "nB must be <> 0"
  | [] , _ -> []
  | e1::l1,e2::l2 when e1 = 1 -> let q = mult_b (quot_n (0::l1) (0::l2)) [1-e2;1] in
                                 if mult_b q bB <> bA then                                
                                   diff_b q [e2;1]
                                 else
                                   q
  | e1::l1,e2::l2 -> mult_b (quot_n (0::l1) (0::l2)) [e2;1];;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = diff_b bA (mult_b (quot_b bA bB) bB);;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = (quot_b bA bB),(mod_b bA bB);;
