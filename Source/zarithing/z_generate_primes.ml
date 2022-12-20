(** Generating primes *)

open Z

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
   A light version done in-class.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n upper limit of elements in the list of big integers.
 *)
let init_eratosthenes n =
   if n < succ one then
    invalid_arg "init_eratosthenes: minimum value is 2"
  else
    let rec init_rec = function
        x when x > n -> []
      | n -> n :: init_rec (n + succ (one))
    in succ one :: init_rec (succ (succ one));;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
   if n < succ one then
    invalid_arg "eratosthenes: minimum value is 2"
  else
    let rec remove_divided x = function
        [] -> []
      | e::l when erem e x = zero -> remove_divided x l
      | e::l -> e :: remove_divided x l
    in let rec era i = function
           [] -> []
         | e::l -> if i = n then
                     [i]
                   else
                     e :: era (i/e) (remove_divided e l) (* reduce rec number *)
       in era (succ one) (init_eratosthenes n);;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::l -> Printf.fprintf oc "%s\n" (to_string e); aux l
  in aux li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (of_string line)::(_create_list in_c)
    | None -> close_in in_c; [] (* Close stream before return *)
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  let ic = open_in file
  in create_list ic;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(* Generating couples of primes numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive big integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec double = function
    | [_] | [] -> []
    | e::l -> let pair = (succ one)*e + one in
              if isprime pair then
                (e,pair) :: double l
              else
                double l
  in double (eratosthenes limit);;
