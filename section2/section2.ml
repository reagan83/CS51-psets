(* CS51 Section 2 *)

(* ********************** Part 0 - A Beautiful Times *************** *)

(* For ps2 we wrote a times function for bignum.  Let's break this function down
 * and make it beautiful.
 *
 * + Times takes two multiply "rows".
 * + Each element of the bottom row is _grouped_ with a copy of the top row.
 * + Each number of the bottom row is _multiplied_ with its grouped copy of the
 *   top row
 * + The resulting rows of multiplied numbers are _staggered_ to give a list of
 *   columns to be added
 * + The staggered columns are _combined_ by adding and carrying when the result
 *   is larger than base
 * 
 * NOTE:
 * + Each of these functions are simple recursive functions with one match
 *   statement.
 * + Each of these functions are _independent_ and can be tested independently.
 * + The only function that needs to know about "math" is
 *   combine_multiplications.  The rest are entirely structural.  Cool!
 *)

(* Returns a bignum representing n1*n2 *)
let times (n1: bignum) (n2: bignum) : bignum =
  (* group pairs numbers on the bottom_row with the entire top row *)
  let rec group (top_row:int list) (bottom_row:int list) : (int * int list) list =
                                                               (* implement me *)

  in
  assert (group [1; 2; 3] [4; 5; 6] = 
                [(4, [1; 2; 3]); (5, [1; 2; 3]); (6, [1; 2; 3])]) ;
  (* multiply_row takes a number and multiplies it to each element in a row *)
  let rec multiply_row (number : int) (row : int list) : int list =
                                                               (* implement me *)

  in
  assert (multiply_row 4 [1; 2; 3] = [4; 8; 12]) ;
  (* multiply_rows performs multiply_row on a list of groupings *)
  let rec multiply_rows (groupings : (int * int list) list) : int list list =
                                                               (* implement me *)

  in
  assert (multiply_rows [(4, [1; 2]); (5, [3; 4])] = [[4; 8]; [15; 20]]) ;
  (* stagger takes a list of rows and returns a list of staggered columns. *)
  let rec stagger (rows : int list list) : int list list =
                                                               (* implement me *)

  (* splice prepends a list element-wise onto a list of lists *)
  and splice (to_splice : int list) (target : int list list) : int list list =
                                                               (* implement me *)

  in
  assert (stagger [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] = 
                  [[1]; [2; 4]; [3; 5; 7]; [6; 8]; [9]]) ;
  assert (splice [1; 2; 3] [[4; 5]; [6; 7]] = [[1; 4; 5]; [2; 6; 7]; [3]]) ;
  (* combine_multiplications sums a list of number columns with a carry *)
  let rec combine_multiplications (ms : int list list) (carry : int) : int list =
                                                               (* implement me *)



  and sum (ns : int list) : int =
                                                               (* implement me *)

  in 
  assert (combine_multiplications [[999; 2; 3]; [999; 3; 4]] 4 = [8; 7; 1]) ;

  (* the entire algorithm for times *)
  let multiplications = 
    multiply_rows (group (List.rev n1.coeffs) (List.rev n2.coeffs)) 
  in
  let new_coeffs = combine_multiplications (stagger multiplications) 0 in
  { coeffs = List.rev new_coeffs; neg = not (n1.neg = n2.neg) }
;;
assert (times (fromInt 1000) (fromInt 24) = fromInt (24000)) ;;

(* ********************** Part 1 - Clean it up. ******************** *)

(* Over the course of your programming career, you will undoubtedly encounter
 * some poorly written code that is hard to read, hard to reason about, and 
 * hard to modify.  With high probability, some of it will be your own code 
 * that you're returning to six months after writing it :)  This exercise
 * is here to practice rewriting code to make it better, while at the same time
 * making sure that your new code has the same behavior as the old.
 *)

(* John Hacker recently learned OCaml, but has not taken CS51, and so
 * he doesn't know about map, reduce or proper style. As a result, he
 * isn't aware that this horribly convoluted function he invented can 
 * be written in one short, elegant line of code.
 *
 * Write a function that behaves identically but is simpler and written with
 * correct style. 
 * 
 * Hint: work by generating a series of functions that are simpler, but do 
 * the same thing.  Use tests to double check.
 *)

let rec mystery (lists : 'a list list) =
  if List.length lists = 0 then []
  else if List.length (List.hd lists) = 0 
  then mystery (List.tl lists)
  else if List.length (List.hd lists) = 1
  then let hd = List.hd lists in 
    ((List.hd) hd) :: mystery (List.tl lists)
  else let hd = List.hd lists in
    (List.hd) hd :: (mystery ((List.tl hd)::(List.tl lists)))
;;
 
let mystery' =   (*Your code here.*)
;;

assert (let x = [[];[]] in mystery x = mystery' x);;


(* ***************** Part 2 - Map and reduce ************************ *)
(* Exercises from section 1 for those sections that didn't get
 * a chance to cover map and reduce thoroughly. The subsections match
 * the subsection from section one (i.e. subsection 1c. corresponds to 4c.
 * from section 1 notes).
 *)

(* Map and reduce *)
(* Exercise 1 *)
let rec reduce f u xs =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl);;

let rec map f xs =
  match xs with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl);;
  
(* 1a. Implement length in terms of reduce. 
 * length lst returns the length of lst. length [] = 0. *)
let length (lst: int list) : int =
  (*Your code here.*)
;;

(* 1b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)
let times_3 (lst: int list): int list =
  (*Your code here.*)
;;

(* 1c. Write a function that takes an int and an int list and multiplies every
 * entry in the list by the int. Use map. *)
let times_x (x : int) (lst: int list): int list =
  (*Your code here.*)
;;

(* 1d. Rewrite times_3 in terms of times_x.
 * This should take very little code. *)
let times_3_shorter =  
  (*Your code here.*)
;;

(* 1e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list.  e.g. mult_table [1;2;3] => [[1; 2; 3]; [2; 4; 6]; [3; 6; 9]] *)
let mult_table (lst: int list) : int list list =
  (*Your code here.*)
;;
  
(* 1f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use reduce. *)
let and_list (lst: bool list) : bool =
  (*Your code here.*)
;;

(* 1g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)
let or_list (lst: bool list) : bool =
  (*Your code here.*)
;;
  
(* 1h.	 Write a function that takes a bool list list and returns
 * its value as a boolean expression in conjunctive normal form (CNF).
 * A CNF expression is represented as a series of OR expressions joined
 * together by AND.
 * e.g. (x1 OR x2) AND (x3 OR x4 OR x5) AND (x6).
 * Use map and/or reduce.
 * You may find it helpful to use and_list and or_list. *)
let cnf_list (lst: bool list list) : bool =
  (*Your code here.*)
;;
  
(* 1i. Write a function that takes an expr list and returns true if and only if
 * every expr in the list represents a true Boolean expression. *)
let all_true (lst: expr list) : bool =
  (*Your code here.*)
;;

(* You may find these helper functions from section 1 exercise 3 helpful. *)

let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =
  match (x, y) with
  | (Some x', Some y') -> Some (f x' y')
  | (_, None) -> x
  | (None, _) -> y ;;
 
let min_option x y = calc_option min x y ;;

let max_option x y = calc_option max x y ;;

let and_option x y = calc_option (&&) x y ;;
  
(* 1j. Write and_list to return a bool option,
 * where the empty list yields None. Use reduce. *)
let and_list_smarter (lst: bool list) : bool option =
  (*Your code here.*)
;;

let and_list_smartest (lst: bool list) : bool option =
  (*Your code here.*)
;;

(* 1k. Write max_of_list from section 0:
 * Return the max of a list, or None if the list is empty. *)
let max_of_list (lst:int list) : int option =
  (*Your code here.*)
;;

(* 1l. Write bounds from section 0:
 * Return the min and max of a list, or None if the list is empty. *)
let bounds (lst:int list) : (int option * int option) =
  (*Your code here.*)
;;

(* **************** Part 3 - More Map/Reduce ******************* *)
(* For more practice writing and using higher order functions. *)

(* 2a. implement most of the functions from Part 0 using map and reduce! *)

let group (top_row:int list) (bottom_row:int list) : (int * int list) list =
  (* implement me *)
;;
let multiply_row (number : int) (row : int list) : int list =
  (* implement me *)
;;
let multiply_rows (groupings : (int * int list) list) : int list list =
  (* implement me *)
;;
let stagger (rows : int list list) : int list list =
  (* implement me *)
;;
let sum (ns : int list) : int =
  (* implement me *)
;;

(* 2b. filtermap 
 * Write a function that takes
 *    -> a predicate, pred
 *    -> a one argument function f with argument type 'a
 *    -> a list of ('a)s, lst
 * The function should: filter out items that make pred false, and
 * return the result of applying f on each element of the remaining
 * list.
 *
 * Your solution should use reduce.
 *)
let filtermap (pred: 'a -> bool) (f: 'a -> 'b) (lst: 'a list) : 'b list = 
  (*Your code here.*)
;;
   
(* 2c.  Use filtermap to write the deoptionalize function from PS2. 
   As a reminder:
   deoptionalize [None; Some 2; None; Some 3; Some 4; None] = [2;3;4] *)
let deoptionalize =   
  (*Your code here.*)
;;

(* You may have noticed that you needed to raise an exception to make
   deoptionalize work properly with arbitrary option types. There are
   very few situations where you shouldn't be doing a complete match, 
   and where you should fall back on exceptions. Here is an alternative 
   (much better) way to define filter_map that avoids this problem. Try
   filling in the code for this definition (use reduce here too) *)

let filter_map (f: 'a -> 'b option) (lst: 'a list) : 'b list = 
  (*Your code here.*)
;;


(* Now write deoptionalize using this new filter_map *)
let deoptionalize' =   
  (*Your code here.*)
;;

(* ******************* Part 4 - Substitution Model ******************* *)
(* The purpose of these exercises is to help you develop a more formal
 * model of how ocaml code evaluates.  This is useful both when writing
 * and reading programs.  For some relevant examples, see the entries
 * at the Underhanded C contest:
 * http://underhanded.xcott.com/?page_id=5 
 *)

(* For each of these, replace ??? with a string that will make the 
 * snippet evaluate to the integer 42.  The string should be properly
 * parenthesized--things like ") in blah blah in blah (" are not allowed.
 *
 * If this is not possible, justify why not.  "I couldn't figure out how" isn't
 * a valid justification.  "No matter what you replace ??? with, this expression
 * cannot possibly evaluate to 42 because..."  is a good start. 
 *)

let reduce f u xs = List.fold_right f xs u;;

reduce (fun x r -> r * 10 + x) 0 [2; 4];;

(* 42.1 *)
let f = ??? in
  f (42, 24)
;;


(* 42.2 *)
let x = ??? in
  let x' = (fun x -> let x = x * 2 in x) in
    reduce (+) 0 (List.filter (fun x -> x = x) [x' x; x])
;;

(* 42.3 *)
let f = (fun x y -> x + y) in
  let g = f ??? in
    g 21
;;

(* 42.5 *)
let f = (fun (x,y) -> x + y) in
let g = f (???) in
  g 21
;;

(* 42.6.1 *)
let f = ??? in
  f f (f f)
;;

(* 42.6.2 *)
let f = ??? in
  (f f) f f
;;

(* From now on, your definitions cannot contain the value "42"! *)

(* 42.7 *)
let f = ??? in
  reduce f 21 [f] ;;


(* 42.x: Bonus *)
let thequestion = ??? in
  6 * thequestion
;;
