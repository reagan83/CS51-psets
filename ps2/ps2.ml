exception ImplementMe;;

(**************************** Part 1: Bignums *********************************)
type bignum = {neg: bool; coeffs: int list};;

let base = 1000;;


(*>* Problem 1.1 *>*)
(* Representation invariant:
 * WRITE YOUR REPRESENTATION INVARIANT HERE. *)

(* Returns true if bignum b obeys the representation invariant above. *)
let repOK (b: bignum) : bool =
  raise ImplementMe
;;
(* Your tests may assume that base = 1000, as the test below does. *)
(* assert (repOK {neg = false; coeffs = [99; 1; 0]});; *)

(*>* Problem 1.2 *>*)
let toInt (b: bignum) : int option =
  raise ImplementMe
;;

let fromInt (n: int) : bignum =
  raise ImplementMe
;;


(** Some helpful string functions **)
(* Splits a string into a list of its characters. *)
let rec explode (s: string) : char list =
  let len = String.length s in
  if len = 0 then []
  else (s.[0])::(explode (String.sub s 1 (len - 1)))

(* Condenses a list of characters into a string. *)
let rec implode (cs: char list) : string =
  match cs with
    | [] -> ""
    | c::t -> (String.make 1 c)^(implode t)

(** Other functions you may find useful. *)
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l: 'a list) (n: int): 'a list =
  match l with
    | [] -> []
    | h::t -> if n <= 0 then [] else h::(take_first t (n - 1))

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h::t -> let (lst1, lst2) = split t (n-1) in
        (h::lst1, lst2)

(* Removes zero coefficients from the beginning of the bignum representation *)
let rec stripzeroes (b: int list) : int list =
  match b with
    | 0::t -> stripzeroes t
    | _ -> b

(* Returns the floor of the base 10 log of an integer *)
let intlog (base: int): int =
  int_of_float (log10 (float_of_int base))
;;

(* fromString and toString assume the base is a power of 10 *) 
(* Converts a string representing an integer to a bignum. *)
let fromString (s: string): bignum =
  let rec fromString_rec (cs: char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
      (int_of_string string_to_convert)::(fromString_rec rest)
  in
  match explode s with
    | [] -> fromInt 0
    | h::t -> if (h = '-')||(h = '~') then 
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false; coeffs = (List.rev (fromString_rec (List.rev (h::t))))}
;;

(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b: bignum): string =
  let rec pad_with_zeroes_left (s: string) (len: int) =
    if (String.length s) >= len then s else
      "0"^(pad_with_zeroes_left s (len - 1))
  in
  let rec stripstrzeroes (s: string) (c: char) =
    if String.length s = 0 then "0" else
    if (String.get s 0) = '0' then 
      stripstrzeroes (String.sub s 1 ((String.length s) - 1)) c
    else s
  in
  let rec coeffs_to_string (coeffs: int list): string =
    match coeffs with
      | [] -> ""
      | h::t -> (pad_with_zeroes_left (string_of_int h) (intlog base))^
          (coeffs_to_string t)
  in
  let stripped = stripzeroes b.coeffs in
    if List.length stripped = 0 then "0" else
      let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
        if b.neg then "~"^from_coeffs else from_coeffs
;;

(*>* Problem 1.3 *>*)
(* Returns true if two bignums are equal, false otherwise. *)
let equal (b1: bignum) (b2: bignum) : bool =
  raise ImplementMe
;;

(* Returns true if b1 is less than b2, false otherwise. *)
let less (b1: bignum) (b2: bignum) : bool =
  raise ImplementMe
;;

(** Some arithmetic functions **)

(* Returns the negation of a bignum (i.e. b*(-1)) *)
let negate (b: bignum) : bignum =
  raise ImplementMe
;;

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1: bignum) (b2: bignum) : bignum =
  let pair_from_carry (carry: int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry: int) 
      : (bool * int list) =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1::t1, h2::t2) -> 
          let (sign1, sign2) = 
            (if neg1 then -1 else 1), (if neg2 then -1 else 1) in
          let result = h1*sign1 + h2*sign2 + carry in
          if result < 0 then 
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, (result+base)::coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, (result-base)::coeffsres)
          else 
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result::coeffsres)
  in
  let (negres, coeffsres) = 
        plus_with_carry (b1.neg, List.rev (stripzeroes b1.coeffs))
          (b2.neg, List.rev (stripzeroes b2.coeffs)) 0
  in {neg = negres; coeffs = List.rev coeffsres}
;;

(*>* Problem 1.4 *>*)
(* Returns a bignum representing b1 + b2.
 * Does not make the above assumption. *)
let plus (b1: bignum) (b2: bignum) : bignum =
  raise ImplementMe
;;

(*>* Problem 1.5 *>*)
(* Returns a bignum representing b1*b2 *)
let times (b1: bignum) (b2: bignum) : bignum =
  raise ImplementMe
;;

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b: int list) (n: int) : int list * int =
  let rec divsing_rec (b: int list) (r: int) : int list * int =
    match b with
      | [] -> [], r
      | h::t -> 
          let dividend = r*base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot*n) in
            (quot::q, r)
  in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1::h2::t -> if h1 < n then divsing_rec ((h1*base + h2)::t) 0
        else divsing_rec b 0
;;

(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let rec divmod (b1: bignum) (b2: bignum): bignum * bignum =
  let rec divmod_rec m n (psum: bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns::_ -> let (p, _) =

            if ns + 1 = base then 
              take_first mc ((List.length mc) - (List.length nc)), 0
            else 
              let den = ns + 1 in
              let num = take_first mc ((List.length mc) - (List.length nc) + 1)
              in divsing num den 
          in
          let bp = {neg = false; coeffs = p} in
          let p2 = if equal bp (fromInt 0) then (fromInt 1) else bp in
            divmod_rec (plus m (negate (times n p2))) n (plus psum p2)
  in
    divmod_rec b1 b2 (fromInt 0)


(*********************** Part 2: The RSA Cryptosystem *************************)

(** Support code for RSA **)

(* Returns a random bignum from 0 to bound - 1 (inclusive). *)
let randbignum (bound: bignum) =
  let rec randbignum_rec (bound: int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _::t -> (Random.int base)::(randbignum_rec t)
  in {neg = false; coeffs = List.rev (randbignum_rec (List.rev bound.coeffs))}
;;

(* Returns b to the power of e mod m *)
let rec expmod (b: bignum) (e: bignum) (m: bignum): bignum =
  if equal e (fromInt 0) then (fromInt 1) else if equal e (fromInt 1) then 
    let (_, x) = divmod b m in x
  else 
    let (q, r) = divmod e (fromInt 2) in
    let res = expmod b q m in
    let (_, x) = divmod (times (times res res) (expmod b r m)) m in 
      {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b: bignum) (e: bignum): bignum =
  if equal e (fromInt 0) then (fromInt 1) else if equal e (fromInt 1) then b
  else 
    let (q, r) = divmod e (fromInt 2) in
    let res = exponent b q in
    let exp = (times (times res res) (exponent b r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n: bignum): bool =
  let rec miller_rabin (k: int) (d: bignum) (s: int): bool =
    if k < 0 then true else
    let rec square (r: int) (x: bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in
        
        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if (equal x (fromInt 1)) || (equal x (plus n (fromInt (-1)))) then 
        miller_rabin (k - 1) d s
      else square 1 x
  in 
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n: bignum) (s: int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m: bignum) (d: bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in 
      (t, plus s (negate (times q t)), g)
;;

(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min: bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max
;;

(** Code for encrypting and decrypting messages using RSA **)

(* Generate a random RSA key pair, returned as (e, d, n). 
 * p and q will be between 2^n and 2^(n+1).
 * Recall that (n, e) is the public key, and (n, d) is the private key. *)
let rec generateKeyPair (r: bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (e, d, times p q) else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair ()
;;

(*>* Problem 2.1 *>*)
(* Encrypts or Decrypts a bignum s using RSA.
   To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n: bignum) (e: bignum) (s: bignum) : bignum =
  raise ImplementMe
;;

(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst: char list) (m: int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c::t -> plus (times (encchars t) (fromInt 256)) (fromInt (Char.code c))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
          (encchars enclist)::(charsToBignums rest m)
;;

(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst: bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> (Char.chr ir)::(decbignum q)
  in
    match lst with
      | [] -> []
      | b::t -> (decbignum b)@(bignumsToChars t)
;;

(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n: bignum) =
  int_of_float ((float_of_int ((List.length (stripzeroes n.coeffs)) - 1)) 
                /. ((log10 2.) *. 8.))
;;

(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n: bignum) (e: bignum) (lst: bignum list) =
  match lst with
    | [] -> []
    | h::t -> (encryptDecryptBignum n e h)::(encDecBignumList n e t)
;;


(*>* Problem 2.2 *>*)
(* Encrypt a string, and return the encrypted message as a list of bignums *)
let encrypt (n: bignum) (e: bignum) (s: string) =
  raise ImplementMe
;;

(* Decrypt an encrypted message (list of bignums) to produce the 
 * original string. *)
let decrypt (n: bignum) (d: bignum) (m: bignum list) =
  raise ImplementMe
;;

(*>* Problem 3.1 *>*)
(* Challenge! (see writeup) *)
(* Returns a bignum representing b1*b2 *)
let times_faster (b1: bignum) (b2: bignum) : bignum =
  raise ImplementMe
;;

let minutes_spent = 0;;
