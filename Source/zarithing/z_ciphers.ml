(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let n = p*q and phi = (p-one)*(q-one) in
  let e = phi/(succ one)-one in
  let d = invert e phi
  in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = powm_sec m e n;;

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = powm_sec m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let rec gen g =
    if erem (g*g) p <> one then
      (g,p)
    else
      gen (of_int (Random.int (to_int (p-(succ one)))) + succ one)
  in gen (of_int (Random.int (to_int (p-(succ one)))) + succ one);;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = of_int (Random.int (to_int((of_int 10)**7 - (of_int 10)**6)))
          + (of_int 10)**6 in
  let priv = powm_sec g a p in
  (priv,a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = of_int (Random.int (to_int((of_int 10)**7 - (of_int 10)**6)))
          + (of_int 10)**6 in
  let c1 = powm_sec g k p and c2 = msg * powm_sec kA k p in
  (c1,c2);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = erem (msgB/powm_sec msgA a p) p;;
