(*** CS 51 Problem Set 1 ***)
(*** YOUR NAME HERE ***)

(* Problem 1 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(*>* Problem 1a *>*)
(*
let prob1a : string  = let greet y = "Hello " ^ y in greet "World!" ;;
*)

(*>* Problem 1b *>*)
(*
let prob1b : int option list = [Some 4; Some 2; None; Some 3] ;;
*)

(*>* Problem 1c *>*)
(*
let prob1c : ('a option * float option)*bool  = ((None, Some 42.0), true) ;;
*)


(* Explain in a comment why the following will not type check,
   and provide a fix *)

(*>* Problem 1d *>*)
(*
let prob1d : string * int list = [("CS", 51); ("CS", 50)] ;;
*)
(* This won't compile because it is a list of tuples of type string * int, and
therefore should be '(string * int) list', otherwise it would be a tuple with a
string AND a list of ints.
ANSWER:
 *)
(*
let prob1d : (string * int) list = [("CS",51); ("CS", 50)];;
*)

(*>* Problem 1e *>*)
(*
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4, 3.9) then 4 else 2;;
*)
(*
 You can't compare different types. Change 3.9 to an int or 4 to a float.
ANSWER:
*)
(*
let prob1e : int =
  let compare (x,y) = x < y in
  if compare (4.0, 3.9) then 4 else 2;;
*)

(*>* Problem 1f *>*)
(*
let prob1f : (string * string) list =
  [("January", None); ("February", 1); ("March", None); ("April", None);
   ("May", None); ("June", 1); ("July", None); ("August", None);
   ("September", 3); ("October", 1); ("November", 2); ("December", 3)] ;;
*)
(*Lists must be of the same type!
ANSWER:
*)
(*
 let prob1f : (string * int option) list =
[("January", None); ("February", Some(1)); ("March", None); ("April", None);
   ("May", None); ("June", Some(3)); ("July", None); ("August", None);
   ("September", Some(3)); ("October", Some(1)); ("November", Some(2));
   ("December", Some(3))] ;;
*)

(* Problem 2 - Fill in expressions to satisfy the following types:
 *
 * NOTE: for option and list types, you must go at least one layer deep.
 * example problems:
 *   let x : int option = ??? ;;
 *   let y : int list = ??? ;;
 * incorrect answers:
 *   let x : int option = None ;;
 *   let y : int list = [] ;;
 * possible correct answers:
 *   let x : int option = Some 1 ;;
 *   let y : int list = [1] ;;
 *   let y : int list = [1; 2] ;;
 *   let y : int list = 1 :: [] ;;
 *)

(*>* Problem 2a *>*)
(*
let prob2a : (int * (string * float) option list) list =
  ???
;;
ANSWER:
*)
(*
let prob2a : (int * (string * float) option list) list = 
[5, [Some ("HELLO", 2.5);None];
 2, [Some ("good", 42.0); Some ("WORLD", 3.6)]];;
*)

(*>* Problem 2b *>*)
(*
type student = {name: string; year: int option; house: string option};;
let prob2b : (student list option * int) list =
  ???
;;
ANSWER:
*)
(*
type student = {name: string; year: int option; house: string option};;
let prob2b : (student list option * int) list =
 [Some [{name="Rob Bowden";year=Some 2013;house=Some "The internet"}], 1;
 Some [{name="Arian Allenson M. Valdez";year=None;house=Some "The internet"}], 2];;
*)


(*>* Problem 2c *>*)
(*
let prob2c : (float * float -> float) * (int -> int -> unit) * bool  =
  ???
;;
ANSWER:
*)
(*
let prob2c : (float * float -> float) * (int -> int -> unit) * bool  =
    let prob2cf ((x,y) : float * float) = y in
    let prob2ci = fun (x : int) -> fun (y : int) -> () in
    (prob2cf, prob2ci, true);; 
*)


(* Fill in ??? with something that makes these typecheck: *)
(*>* Problem 2d *>*)
(*
let prob2d =
  let rec foo bar =
    match bar with
    | (a, (b, c)) :: xs -> if a then (b - c + (foo xs)) else foo xs
    | _ -> 0
  in foo ???
;;
ANSWER:
*)
(*
let prob2d =
  let rec foo bar =
    match bar with
    | (a, (b, c)) :: xs -> if a then (b - c + (foo xs)) else foo xs
    | _ -> 0
  in foo ;;
*)
(*I'm not sure wheter this is the correct answer :) 
However, the requirement is that the code type-checks correctly, and it does.*)

(*>* Problem 2e *>*)
(*
let prob2e =
  let v = (32.0, 28.0) in
  let square x = x *. x in
  let squared_norm (w: float * float) : float = ??? in
  let d = sqrt (squared_norm v) in
  int_of_float d
;;
*)
(*
let prob2e =
  let v = (32.0, 28.0) in
  let square x = x *. x in
  let squared_norm (w: float * float) : float = 
     match w with
     |(x,y) -> square x +. square y
  in
  let d = sqrt (squared_norm v) in
  int_of_float d
*)



(* Problem 3 - Write the following functions *)
(* For each subproblem, you must implement a given function and corresponding
 * unit tests.  You are provided a high level description as well as a
 * prototype of the function you must implement. *)

(*>* Problem 3a *>*)

(* reversed lst should return [true] if the integers in the list are in
 * decreasing order. The empty list is considered to be reversed. *)

(* Here is its prototype/signature: *)
(* reversed : int list -> bool *)

(* Implement reversed below, and be sure to write tests for it (see 3b for
 * examples of tests). *)

(*
let rec reversed (x:int list):bool =
   match x with
   | [] -> true
   | y::z::tl -> if y < z then false else reversed (z::tl);
   | y::[] -> true;; *)

(*
(* tests *)
assert ((reversed [1;2;3;4;5;6;7;8]) = false);;
assert ((reversed [8;9;7;6;5;4;3;2;1]) = false);;
assert ((reversed [9;8;7;6;4;5;3;1;2]) = false);;
assert ((reversed [9;8;7;6;5;4;3;2;1]) = true);;
assert ((reversed [3;2;1]) = true);;
*)

(*>* Problem 3b *>*)

(* merge xs ys takes two integer lists, each sorted in increasing order,
 and returns a single merged list in sorted order.  For example:
# merge [1;3;5] [2;4;6];;
- : int list = [1; 2; 3; 4; 5; 6]
# merge [1;3;5] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12]
# merge [1;3;5;700;702] [2;4;6;12];;
- : int list = [1; 2; 3; 4; 5; 6; 12; 700; 702]
*)
 
(* The type signature for [merge] is as follows: *)
(* merge : int list -> int list -> int list *)

(*
let rec merge (x:int list) (y:int list) =
   match x, y with
   |[],_ -> x
   |_,[] -> y
   |x::xtl, y::ytl -> if x < y then x::y::(merge xtl ytl) else y::x::(merge xtl ytl);; 
*)

(*
(* sample tests *)
assert ((merge [1;2;3] [4;5;6;7]) = [1;2;3;4;5;6;7]) ;;
assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]) ;;
assert ((merge [4;5;6;7] [1;2;3]) = [1;2;3;4;5;6;7]) ;;
assert ((merge [2;2;2;2] [1;2;3]) = [1;2;2;2;2;2;3]) ;;
assert ((merge [1;2] [1;2]) = [1;1;2;2]) ;;
assert ((merge [-1;2;3;100] [-1;5;1001]) = [-1;-1;2;3;5;100;1001]) ;;
assert ((merge [] []) = []) ;;
assert ((merge [1] []) = [1]) ;;
assert ((merge [] [-1]) = [-1]) ;;
assert ((merge [1] [-1]) = [-1;1]) ;;
*)



(*>* Problem 3c *>*)
(* unzip should be a function which, given a list of pairs, returns a
   pair of lists, the first of which contains each first element of
   each pair, and the second of which contains each second element.
   The returned lists should have the elements in the order that they
   appeared in the input.  So, for instance,
   unzip [(1,2);(3,4);(5,6)] = ([1;3;5],[2;4;6])
*)

(* The type signature for unzip is as follows: *)
(* unzip : (int * int) list -> int list * int list) *)

(*
let rec unzip (data:(int*int) list) : int list * int list =
   match data with
   |(x,y)::tl->let l, r = unzip tl in (x::l, y::r)
   |[]->([],[])
in

(* tests *)
assert (unzip [(1,2);(3,4);(5,6)] = ([1;3;5],[2;4;6]));;
assert (unzip [(5,6);(7,8);(9,10)] = ([5;7;9],[6;8;10]));;
assert (unzip [(6,7)] = ([6],[7]));;
*)

(*>* Problem 3d *>*)

(* variance lst should be a function that returns None if lst has fewer than 2
floats, and Some of the variance of the floats in lst otherwise.  Recall that
the variance of a sequence of numbers is 1/(n-1) * sum (x_i-m)^2, where
 a^2 means a squared, and m is the arithmetic mean of the list (sum of list /
length of list).  For example,
 - variance [1.0; 2.0; 3.0; 4.0; 5.0] = Some 2.5
 - variance [1.0] = None

Remember to use the floating point version of the arithmetic operators when
operating on floats (+. *., etc).  The "float" function can cast an int to a
float.
*)

(* variance : float list -> float option *)
(* Notes: This was quite scary to implement hohoho
   No List Module!!! :o Have to implement my own list function!
 *)
(*
let variance (sample:float list) : float option = 
  let rec summation (ulist: float list) : float = 
    match ulist with
    |[]->0.0
    |x::tl -> (x +. summation tl)
  in
  let rec length (ulist:float list) : int =
    match ulist with
    |[]->0
    |x::tl-> 1 + length tl
  in
  let mean (floats:float list) : float =
    match floats with
    |[]->0.0
    |x::tl-> summation floats /. float (length floats)
  in
  let meanValue = mean sample in 
  let rec squareDifference (test: float list) : float list =
    match test with
    |[]->[]
    |x::tl -> (x-.meanValue)**2.0::squareDifference tl
  in
  match sample with
  |[]->None
  |hd::[]->None
  |hd::tl->Some (summation (squareDifference sample) /. float (length sample - 1))
in


(* tests *)
assert (variance [1.0;2.0;3.0;4.0;5.0] = Some 2.5);;
assert (variance [1.0;2.0] = Some 0.5);;
assert (variance [1.0] = None);;
*)

(*>* Problem 3e *>*)

(* few_divisors n m should return true if n has fewer than m divisors,
 * (including 1 and n) and false otherwise. Note that this is *not* the
 * same as n having fewer divisors than m:
few_divisors 17 3;;
- : bool = true
# few_divisors 4 3;;
- : bool = false
# few_divisors 4 4;;
- : bool = true
 *)
(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)
(*
let few_divisors (n:int) (m:int) = 
  let rec divisors (x:int) (y:int) = 
    let newDivisor = y-1 in
    if y > 0 then
      if x mod y = 0 then 1 + divisors x newDivisor else 0 + divisors x newDivisor
    else 
       0
  in
  if divisors n n < m then 
     true
  else 
     false
in

(* tests *)
assert (few_divisors 17 3 = true);;
assert (few_divisors 4 3 = false);;
assert (few_divisors 4 4 = true);;
*)

(*>* Problem 3f *>*)

(* concat_list sep lst should take a list of strings and return one big
   string with all the string elements of lst concatenated together, but
   separated by the string sep.  Here are some example tests:
concat_list ", " ["Greg"; "Anna"; "David"] ;;
- : string = "Greg, Anna, David"
concat_list "..." ["Moo"; "Baaa"; "Quack"] ;;
- : string = "Moo...Baaa...Quack"
concat_list ", " [] ;;
- : string = "" ;;
concat_list ", " ["Moo"] ;;
- : string = "Moo"
*)
(* The type signature for concat_list is: *)
(* concat_list : string -> string list -> string *)

(*
let rec concat_list (sep:string) (words:string list) =
  match words with
  |hd::[]->hd
  |hd::tl->hd^sep^concat_list sep tl
  |[]->""
in

(* tests *)
assert(concat_list ", " ["Greg"; "Anna"; "David"] = "Greg, Anna, David");;
assert(concat_list "..." ["Moo"; "Baaa"; "Quack"] = "Moo...Baaa...Quack");;
assert(concat_list ", " [] = "");;
assert(concat_list ", " ["Moo"] = "Moo");;
*)

(*>* Problem 3g *>*)

(* One way to compress a list of characters is to use run-length encoding.
  The basic idea is that whenever we have repeated characters in a list
  such as ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] we can
  (sometimes) represent the same information more compactly as a list
  of pairs like [(5,'a');(3,'b');(1,'c');(4,'d')].  Here, the numbers
  represent how many times the character is repeated.  For example,
  the first character in the string is 'a' and it is repeated 5 times,
  followed by 3 occurrences of the character 'b', followed by one 'c',
  and finally 4 copies of 'd'.

  Write a function to_run_length that converts a list of characters into
  the run-length encoding, and then write a function from_run_length
  that converts back.  Writing both functions will make it easier to
  test that you've gotten them right.
*)
(* The type signatures for to_run_length and from_run_length are: *)
(* to_run_length : char list -> (int * char) list *)
(* from_run_length : (int * char) list -> char list *)

(*
let to_run_length (lst : char list) : (int*char) list =
  let rec compress i s lst1 =
    match lst1 with
    | [] -> [(i,s)]
    | (x::xs) when s <> x ->  (i,s) :: compress 0 x lst1
    | (x::xs) -> compress (i + 1) s xs
  in
  match lst with
  | x :: xs -> compress 0 x lst
  | [] -> []

let from_run_length (lst:(int * char) list) : char list =
  let rec uncompress cnt ltr :char list =
    if cnt > 0 then ltr :: uncompress (cnt - 1) ltr else []
  in
  match lst with
  |(i,c)::tl -> uncompress i c @ from_run_length tl
  |_ -> []

(* tests *)
assert(to_run_length ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d'] =
                     [(5,'a');(3,'b');(1,'c');4,'d');;
assert(to_run_length ['a';'a';'b';'b'] = [(2,'a');(2,'b')];;
assert(from_run_length [(5,'a');(3,'b');(1,'c');4,'d') = 
                  ['a';'a';'a';'a';'a';'b';'b';'b';'c';'d';'d';'d';'d']);;
assert(from_run_length [(2,'a');(2,'b')] = ['a';'a';'b';'b']);;
*)

(*
let rec unzip (data:(int*int) list) : int list * int list =
   match data with
   |(x,y)::tl->let l, r = unzip tl in (x::l, y::r)
   |[]->([],[])
*)


(*>* Problem 4 *>*)

(* Challenge!

  permutations lst should return a list containing every
  permutation of lst.  For example, one correct answer to
  permutations [1; 2; 3] is
  [[1; 2; 3]; [2; 1; 3]; [2; 3; 1]; [1; 3; 2];	[3; 1; 2]; [3; 2; 1]].

  It doesn't matter what order the permutations appear in the returned list.
  Note that if the input list is of length n then the answer should be of
  length n!.

  Hint:
  One way to do this is to write an auxiliary function,
  interleave : int -> int list -> int list list,
  that yields all interleavings of its first argument into its second:
  interleave 1 [2;3] = [ [1;2;3]; [2;1;3]; [2;3;1] ].
  You may also find occasion for the library functions
  List.map and List.concat.
*)

(* The type signature for permuations is: *)
(* permutations : int list -> int list list *)
(*
let rec perms l = 
   let rec interleave x l = 
     match l with
      |[]->[[x]]
      |f::tl -> (x::l)::(List.map (fun p -> f::p) (interleave x tl))
     in
   match l with
   |hd::tl -> List.flatten (List.map (interleave hd) (perms tl))
   |_ -> [l]
in

(* tests *)
assert(perms [1;2;3] = [[1;2;3]; [2;1;3]; [2;3;1]; [1;3;2]; [3;1;2]; [3;2;1]]);;
*)
