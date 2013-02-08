
(* TESTING REQUIRED:
 * Ocaml provides the function "assert" which takes a bool. It does nothing if
 * the bool is true and throws an error if the bool is false.
 *
 * To develop good testing practices, we expect at least 2 tests (using assert)
 * per function that you write on this problem set. Please follow the model
 * shown in 1.2.a, putting the tests just below the function being tested.
 *)


(* TIME ADVICE:
 * Part 2 of this problem set (expression.ml) can be difficult, so be careful
 * with your time. *)


(* HINT: This function should prove useful! *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
    match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl)
;;



(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(*>* Problem 1.1.a *>*)

(*  reduce_mine : Implement reduce using List.fold_right *)
let reduce_mine (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
    raise (Failure "Not implemented")
;;



(****************************************************)
(******       1.2: Sparking your INTerest      ******)
(****************************************************)

(* Solve each problem in this part using List.map, reduce or List.filter.
 * See the Ocaml Standard Library documentation on lists:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *
 * A solution, even a working one, that does not use one of these
 * higher-order functions, will receive little or no credit.
 * However, if you can express your solution to
 * one particular part in terms of another function from
 * another part, you may do so.
 *
 * You MAY NOT change the definition of these
 * functions to make them recursive. *)

(*>* Problem 1.2.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
    raise (Failure "Not implemented")
;;

(* Unit test example. *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]) ;;


(*>* Problem 1.2.b *>*)

(*  sum : Returns the sum of the elements in the list. *)
let sum (nums:int list) : int =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.2.c *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.2.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list.
 *     Example : filter_odd [1;4;5;-3] = [1;5;-3]. *)
let filter_odd (nums:int list) : int list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.2.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.2.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
    raise (Failure "Not implemented")
;;

(*>* Problem 1.2.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list.
 *       Example : filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let filter_range (nums:int list) (range:int * int) : int list =
    raise (Failure "Not implemented")
;;



(****************************************************)
(**********       1.3 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.3.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)
let floats_of_ints (nums:int list) : float list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None.
 *  Example : log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None] *)
let log10s (lst: float list) : float option list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.c *>*)

(*  deoptionalize : Extracts values from a list of options.
 *        Example : deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)
let deoptionalize (lst:'a option list) : 'a list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options;
 *             ignores None values *)
let some_sum (nums:int option list) : int =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.e *>*)

(*  mult_odds : Product of all of the odd members of a list.
 *    Example : mult_odds [1;3;0;2;-5] = -15 *)
let mult_odds (nums:int list) : int =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.3.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(*  filter_by_year : returns the names of the students in a given year
 *         Example : let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *                   filter_by_year students 2010 => ["Joe";"Bob"] *)
let filter_by_year (slist:student list) (yr:year) : name list =
    raise (Failure "Not implemented")
;;


(*>* Problem 1.4 *>*)

(* Please give us an honest estimate of how long this Part of the problem
 * set took you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent_on_part_1 : int = raise (Failure "Not implemented") ;;
