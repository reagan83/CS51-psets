(* CS51 Section 0: Intro to ML
 *
 * Exercises: The purpose of these exercises is to help you start
 * getting comfortable with Ocaml.  The focus is on pattern matching,
 * list operations, and a bit of basic arithmetic.
 *
 * A goal throughout the semester will be writing code that's clear,
 * concise, and beautiful -- not just correct.  Try to make your
 * solutions as simple possible.  Once you have a version that works,
 * look for ways to simplify it. *)

(* Make it so that that x equals 42, by adding 22 to 20 *)
let x = (* Your code here. *) ;;

(* Make it so that x1 equals 42.0, by adding 2 numbers. *)
let x1 = (* Your code here. *) ;;

(* Write a function takes a string, and appends
 * ", and that is why I love CS51" to the end of it. *)
let cs51_loveifier input = (* Your code here *) ;;

(* Write a function that takes a number and returns
 * the difference between that number and 42.
 * Eg, if 'num' is 50, the result should be 8.
 * If 'num' is 30, the result should be -12 *)
let difference_between_x_and_42 num = (* Your code here *) ;;


(* One more simple arithmetic example...
 * Write a function that returns the volume of a cylinder
 * with height h, radius r. *)
let volume_cylinder (h:float) (r:float) : float =
  (* Your code here *) ;;


(* Here, you might have a solution in mind, but not know how to
 * implement it in OCaml.  See if you can Google for how to do it... *)
let even (x: int) : bool =
    (* Your code here *) ;;

(* Can you write odd /in terms of/ even? *)
let odd (x: int) : bool =
    (* Your code here *) ;;

(* OCaml comes pre-packaged with a standard library, that includes
 * a lot of utility functions that you don't have to write yourself.
 * For instance, check out the String module
 * (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html)
 *
 * Now... write a function that takes a String, and returns whether
 * or not that String is more than 10 characters long. *)
let is_more_than_10_characters_long str = (* Your code here. *) ;;


(* LISTS *)
(* We're going to introduce some simple lists.
 * To start, make 'l1' be a list of 3, followed by 4, followed by 5. *)
let l1 = (* Your code here *) ;;


(* Try to make l2 be a list of 4, followed by "Greg".  Does it work?
 * Why or why not?
 * If it doesn't work, just make l2 be the empty list. *)
let l2 = (* Your code here *) ;;

(* Now we're going to do some basic matching on lists.
 *
 * It's fun to get the hang of 'match'.  In some ways,
 * a 'match' can be thought of as analogous to an 'if' statement
 * or a 'switch' statement, in that you're choosing which branch
 * of code to follow, based on the value of a variable.
 *
 * Here's a really simple example is_empty that takes a list, and returns
 * true if the list is empty, and false if the list is not empty. *)
let list_is_empty lst =
    match lst with
    | [] -> true
    | _ :: _ -> false

(* Now, see if you can tackle the following functions... *)

(* Return the head of a list, or None if empty. *)
let head (x:int list) : int option =
  (* Your code here *) ;;

(* Return the tail of a list, or None if empty. *)
let tail (x:int list) : int list option =
  (* Your code here *) ;;

(* Return the last int of an int list, or None if empty. *)
let last_number (x:int list) : int option =
  (* Your code here *) ;;

(* Retain only even integers *)
let rec filter_even (l:int list) : int list =
  (* Your code here *) ;;

(* Square all the elements of a list. *)
let rec square_all (a:int list) : int list =
  (* Your code here *) ;;

(* Return the max of a list, or None if the list is empty. *)
(* Note: Might be good to walk through this in English before syntactifying *)
let rec max_of_list (x:int list) : int option =
  (* Your code here *) ;;

(* Return the min and max of a list, or None if the list is empty. *)
let rec bounds (x:int list) : (int * int) option =
  (* Your code here *) ;;

(* From a list of pairs, retain only those that are in order. *)
let rec proper_pairs (l:(int * int) list) : (int * int) list =
  (* Your code here *) ;;

(* Zip three lists. Return None if different lengths. *)
let rec threezip (a:int list) (b:int list) (c:int list) :
    ((int * int * int) list) option =
  (* Your code here *) ;;


(* Compute the dot product of two lists.
 * Use zip, prods from lecture, write sum. *)

let rec prods (l: (int*int) list) : int list =
  match l with
    | [] -> []
    | (x,y) :: tl -> (x*y) :: (prods tl)
;;

let rec zip (x:int list) (y:int list) : ((int*int) list) option =
  match (x,y) with
    | ([], []) -> Some []
    | (xhd::xtl, yhd::ytl) ->
        (match zip xtl ytl with
           | None -> None
           | Some ztl -> Some ((xhd,yhd)::ztl))
    | (_, _) -> None
;;

let rec sum (l:int list) : int =
  (* Your code here *) ;;

let rec dotproduct (a:int list) (b:int list) : int option =
  (* Your code here *) ;;

(* Given a matrix (list of lists), return the transpose.
 * The transpose of a matrix interchanges the rows and columns.
 * For example, transpose [[1;2;3];[4;5;6]];;
 * where [1;2;3] and [4;5;6] are the rows,
 * should return [[1;4];[2;5];[3;6]].
 *
 * Hint: write an auxiliary function, split, that
 * returns the first column of a matrix as a list
 * and the rest of the matrix as a list of rows.
 *
 * For now, don't worry about doing anything smart if the input
 * isn't a valid matrix.
 *)

let rec split (m:int list list) : (int list * int list list) option =
  (* Your code here *) ;;

let rec transpose (m:int list list) : int list list =
  (* Your code here *) ;;
