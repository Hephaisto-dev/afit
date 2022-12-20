(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n = 
  if n << [0;0;1] then
    invalid_arg "init_eratosthenes: minimum value is [0;0;1]"
  else
    let rec init_rec = function
        x when x >> n -> []
      | n -> n :: init_rec (add_b n [0;0;1])
    in [0;0;1] :: init_rec [0;1;1];;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n = 
  if n < [0;0;1] then
    invalid_arg "eratosthenes: minimum value is [0;0;1]"
  else
    let rec remove_divided x = function
        [] -> []
      | e::l when mod_b e x = [] -> remove_divided x l
      | e::l -> e :: remove_divided x l
    in let rec era i = function
           [] -> []
         | e::l -> if i = n then
                     [i]
                   else
                     e :: era (quot_b i e) (remove_divided e l) (* reduce rec number *)
       in era [0;0;1] (init_eratosthenes n);;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = 
  let oc = open_out file in
  let rec line = function
      [] -> ""
    | e::l -> string_of_int e ^ "," ^ line l 
  in let rec aux = function
      [] -> close_out oc
    | e::l -> Printf.fprintf oc "%s\n" (line e); aux l
  in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = 
  let rec extract_list s = function
      i when i >= String.length s -> []
    | i -> (int_of_string (Char.escaped s.[i])) :: extract_list s (i+2)  
  in let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> extract_list line 0::(_create_list in_c)
    | None -> close_in in_c; [] (* Close stream before return *)
  in
   _create_list in_c

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = 
  let ic = open_in file
  in create_list ic;;

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime = 
  let rec double = function
    | [_] | [] -> []
    | e::l -> let pair = add_b (mult_b [0;0;1] e) [0;1] in
              if isprime pair then
                (e,pair) :: double l
              else
                double l
  in double (eratosthenes limit);;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = 
  let rec twin = function
    | [_] | [] -> []
    | e::l -> let pair = add_b e [0;0;1] in
              if isprime pair then
                (e,pair) :: twin l
              else
                twin l
  in twin (eratosthenes limit);;
