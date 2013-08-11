exception ImplementMe;;

(**************************** Part 1: Bignums *********************************)
type bignum = {neg: bool; coeffs: int list};;
let base = 1000;;

(* Please make sure you fully understand the representation invariant for bignums, 
   as documented in the problem set specification. *)

(*>* Problem 1.1 *>*)
let negate (b: bignum) : bignum =
  let {neg=n; coeffs=i} = b in {neg=not n; coeffs=i}
;;

(*>* Problem 1.2 *>*)

let fromInt (n: int) : bignum =
  let rec aux num:int list=
    if num==0 then
       []
    else 
       abs (num mod base) :: aux (num/base)
  in
  {neg=n<0; coeffs=List.rev (aux n)}
;;

(* tests *)
assert(fromInt 123456 = {neg=false; coeffs=[123;456]});;
assert(fromInt 123456789 = {neg=false; coeffs=[123;456;789]});;
assert(fromInt (-123456) = {neg=true; coeffs=[123;456]});;
assert(fromInt (-123456789) = {neg=true;coeffs=[123;456;789]});;

let toInt (b: bignum) =
  let rec aux (b:int list) (p:int)=
     match b with
     |hd::[]-> Some (hd*p)
     |hd::tl-> (match aux tl (p*base) with
        |Some c-> let bint = (hd*p) in 
                  if bint < hd then None 
                  else Some (bint+c)
        |None -> None)
     |[]->Some 0
  in
  let {neg=n; coeffs=x} = b in
  let c = List.rev x in
     match aux c 1 with
     |Some c-> if n then Some (-c) else Some c
     |None -> None
;;

(* tests *)
assert(toInt (fromInt 123456) = Some(123456));;
assert(toInt (fromInt 123456789) = Some (123456789));;
assert(toInt (fromInt max_int) = Some (max_int));;
assert(toInt (fromInt min_int) = Some (min_int));;
assert(toInt {neg=false;coeffs=[123;456;789;123;456;789]} = None);;

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
let equal (b1: bignum) (b2: bignum) : bool =
  b1 = b2
;;

let less (b1: bignum) (b2: bignum) (*: bool =*)=
  let rec valCheck n1 n2=
     match (n1,n2) with
     |(h1::[],h2::[]) -> h1 < h2
     |(h1::t1,h2::t2) -> if h1 < h2 then true
                         else if h1 = h2 then valCheck t1 t2
                         else false
     |([],[])->true
     |(_,_) -> false
  in
  let rec aux n1 n2 =
     if List.length n1 > List.length n2 then false
     else if List.length n1 < List.length n2 then true
     else valCheck n1 n2
  in
  let {neg=n1;coeffs=c1}=b1 in
  let {neg=n2;coeffs=c2}=b2 in
  if n1 != n2 then (if n1 && not n2 then true else false)
  else (if n1 then not (aux c1 c2) else aux c1 c2)
;;

let greater (b1: bignum) (b2: bignum) : bool =
  let rec valCheck n1 n2=
     match (n1,n2) with
     |(h1::[],h2::[]) -> h1 > h2
     |(h1::t1,h2::t2) -> if h1 > h2 then true
                         else if h1 = h2 then valCheck t1 t2
                         else false
     |([],[])->true
     |(_,_) -> false
  in
  let rec aux n1 n2 =
     if List.length n1 > List.length n2 then true
     else if List.length n1 < List.length n2 then false
     else valCheck n1 n2
  in
  let {neg=n1;coeffs=c1}=b1 in
  let {neg=n2;coeffs=c2}=b2 in
  if n1 != n2 then (if n1 && not n2 then false else true)
  else (if n1 then not (aux c1 c2) else aux c1 c2)
;;

(* tests *)
let p1 = fromInt 100;;
let p2 = fromInt 150;;
let n1 = fromInt ~-100;;
let n2 = fromInt ~-150;;
assert(less p1 p2 = greater p2 p1);;
assert(less p2 p1 = greater p1 p2);;
assert(less n1 p1 = greater p1 n1);;
assert(less n2 p1 = greater p1 n2);;
assert(less n1 n2 = greater n2 n1);;
assert(less n2 n1 = greater n1 n2);;

let o1 = fromInt 1500;;
let o2 = fromInt 3000;;
assert(less o1 o2 = true);;

(** Some arithmetic functions **)

 (* Assumes that b1 + b2 > 0. *)
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
        plus_with_carry (b1.neg, List.rev b1.coeffs)
          (b2.neg, List.rev b2.coeffs) 0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)}
;;

(*>* Problem 1.4 *>*)
(* Returns a bignum representing b1 + b2.
 * Does not make the above assumption. *)
let plus (b1: bignum) (b2: bignum) : bignum =
  let {neg=n1;coeffs=c1}=b1 in
  let {neg=n2;coeffs=c2}=b2 in
  if n1=n2 then 
   let {neg=nr;coeffs=cr} = plus_pos {neg=false;coeffs=c1} {neg=false;coeffs=c2} in
   {neg=n1;coeffs=cr}
  else if n1 then
   (if greater (negate b1) b2 then
      negate (plus_pos (negate b1) (negate b2))
    else 
      plus_pos b1 b2
   )
  else
   (if less b1 (negate b2) then
      negate (plus_pos (negate b1) (negate b2))
    else
      plus_pos b1 b2
   )
;;

(* tests *)
assert(plus (fromInt ~-100) (fromInt 50)   = fromInt ~-50);;
assert(plus (fromInt 50) (fromInt ~-100)   = fromInt ~-50);;
assert(plus (fromInt ~-150) (fromInt ~-150)= fromInt ~-300);;
assert(plus (fromInt ~-1250) (fromInt 15) = fromInt ~-1235);;
assert(plus (fromInt 1500) (fromInt 3000) = fromInt 4500);;
assert(plus (fromInt 10020032) (fromInt 324640018) = fromInt 334660050);;

(*>* Problem 1.5 *>*)
(* Returns a bignum representing b1*b2 *)
let times (b1: bignum) (b2: bignum) : bignum =
  let {neg=n1;coeffs=c1}=b1 in
  let {neg=n2;coeffs=c2}=b2 in
  let rec tmsSngle b i c p= 
    if p > 0 then 0::tmsSngle b i c (p-1)
    else
    match b with
    |hd::tl-> let prod=(hd * i)+c in (prod mod base)::(tmsSngle tl i (prod/base) 0)
    |[] -> if c>0 then [c] else []
  in
  let rec aux mcnd mltpr p= 
    match mltpr with
    |hd::tl -> plus ({neg=false;coeffs=List.rev(tmsSngle mcnd hd 0 p)}) 
                    (aux mcnd tl (p+1))
    |[]->{neg=false;coeffs=[]}
  in
  if n1=n2 then aux (List.rev c1) (List.rev c2) 0
  else negate(aux (List.rev c1) (List.rev c2) 0)
;;

(* tests *)
assert (times (fromInt 111222) (fromInt 3) = {neg=false;coeffs=[333;666]});;
assert (times (fromInt 123456) (fromInt 5) = {neg=false;coeffs=[617;280]});;
assert (times (fromInt 123456) (fromInt 250) = {neg=false;coeffs=[30;864;0]});; 
assert (times (fromInt 100) (fromInt 10) = {neg=false;coeffs=[1;0]});;
assert (times (fromInt 1000) (fromInt 3111) = {neg=false;coeffs=[3;111;0]});;
assert (times (fromInt 123456) (fromInt 123456) = 
       {neg=false;coeffs=[15;241;383;936]});;
assert (times ({neg=false;coeffs=[123;456;789]})({neg=false;coeffs=[123;456;789]}) = 
               {neg=false;coeffs=[15;241;578;750;190;521]});;
assert (times (fromInt ~-111222) (fromInt 3) = {neg=true;coeffs=[333;666]});;
assert (times (fromInt ~-111222) (fromInt ~-3) = {neg=false;coeffs=[333;666]});;

let clean (b : bignum) : bignum = 
  {neg = b.neg; coeffs = stripzeroes b.coeffs}
;;

(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n: bignum) =
  int_of_float ((float_of_int ((List.length (stripzeroes n.coeffs)) - 1)) 
                *. (log10 (float_of_int base)) /. ((log10 2.) *. 8.))
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
  let clean_b1, clean_b2 = clean b1, clean b2 in
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
          let bp = clean {neg = false; coeffs = p} in
          let p2 = if equal bp (fromInt 0) then (fromInt 1) else bp in
            divmod_rec (plus m (negate (times n p2))) n (plus psum p2)
  in
    divmod_rec clean_b1 clean_b2 (fromInt 0)
;;


(**************************** Part 2: RSA *********************************)

(** Support code for RSA **)
(* Returns a random bignum from 0 to bound - 1 (inclusive). *)
let randbignum (bound: bignum) =
  let rec randbignum_rec (bound: int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _::t -> (Random.int base)::(randbignum_rec t)
  in {neg = false; coeffs = stripzeroes (List.rev (randbignum_rec (List.rev bound.coeffs)))}
;;

(* Returns b to the power of e mod m *)
let rec expmod (b: bignum) (e: bignum) (m: bignum): bignum =
  let clean_b, clean_e, clean_m = clean b, clean e, clean m in
  if equal clean_e (fromInt 0) then (fromInt 1) else if equal clean_e (fromInt 1) then 
    let (_, x) = divmod clean_b clean_m in x
  else 
    let (q, r) = divmod clean_e (fromInt 2) in
    let res = expmod clean_b q clean_m in
    let (_, x) = divmod (times (times res res) (expmod clean_b r clean_m)) clean_m in 
      {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b: bignum) (e: bignum): bignum =
  let clean_e, clean_b = clean e, clean b in
  if equal clean_e (fromInt 0) then (fromInt 1) else if equal clean_e (fromInt 1) 
  then clean_b
  else 
    let (q, r) = divmod clean_e (fromInt 2) in
    let res = exponent clean_b q in
    let exp = (times (times res res) (exponent clean_b r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n: bignum): bool =
  let clean_n = clean n in
  let rec miller_rabin (k: int) (d: bignum) (s: int): bool =
    if k < 0 then true else
    let rec square (r: int) (x: bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) clean_n in
        
        if equal x (fromInt 1) then false
        else if equal x (plus clean_n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if (equal x (fromInt 1)) || (equal x (plus clean_n (fromInt (-1)))) then 
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
      let (d, s) = factor (plus clean_n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m: bignum) (d: bignum) : bignum * bignum * bignum =
  let clean_m, clean_d = clean m, clean d in
  if equal clean_d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod clean_m clean_d in
    let (s, t, g) = euclid clean_d r in 
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
(* To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n: bignum) (e: bignum) (s: bignum) : bignum =
  expmod s e n
;;

(* tests further down *)

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

(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n: bignum) (e: bignum) (lst: bignum list) =
  match lst with
    | [] -> []
    | h::t -> (encryptDecryptBignum n e h)::(encDecBignumList n e t)
;;

(*>* Problem 2.2 *>*)
let encrypt (n: bignum) (e: bignum) (s: string) =
   encDecBignumList n e (charsToBignums (explode s) (bytesInKey n))
;;

(* Decrypt an encrypted message (list of bignums) to produce the 
 * original string. *)
let decrypt (n: bignum) (d: bignum) (m: bignum list) =
    implode (bignumsToChars (encDecBignumList n d m))
;;

(* tests *)
let (es,ds,ns)=generateKeyPair (fromInt 64);;
assert(encryptDecryptBignum ns ds (encryptDecryptBignum ns es (fromInt 567890)) =
       fromInt 567890);;
assert(encryptDecryptBignum ns ds (encryptDecryptBignum ns es (fromInt 12356789)) =
       fromInt 12356789);;
let message = "I am a Super Secret Message that is to be encoded with RSA 
(made long to test out that it takes into account the modulus m).";;

assert(decrypt ns ds (encrypt ns es message)=message);;

(**************************** Part 3: Challenge *********************************)
(*
(* Returns a bignum representing b1*b2 *)
let times_faster (b1: bignum) (b2: bignum) : bignum =
  raise ImplementMe
;;
*)
let minutes_spent = 330;;
(* I made a slight mistake in the less and greater functions that made me 
hunt which part of my code was wrong. This took a HUGE amount of time I'm
not particularly proud of :/ *)
