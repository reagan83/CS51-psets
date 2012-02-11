
open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionlibrary.ml
 * 3. Remember to test using the function "assert".
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  raise (Failure "Not implemented") ;;



(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built in method of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  raise (Failure "Not implemented") ;;



(*>* Problem 2.3 *>*)

(* See writeup for instructions. We have pictures! *)
let rec derivative (e:expression) : expression =
  match e with
    | Num _ -> Num 0.
    | Var -> raise (Failure "Not implemented")
    | Unop (u,e1) ->
        (match u with
           | Sin -> raise (Failure "Not implemented")
           | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
           | Ln -> raise (Failure "Not implemented")
           | Neg -> Unop(Neg,derivative e1)
	)
    | Binop (b,e1,e2) ->
        match b with
          | Add -> Binop(Add,derivative e1,derivative e2)
          | Sub -> Binop(Sub,derivative e1,derivative e2)
          | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                         Binop(Mul,derivative e1,e2))
          | Div -> raise (Failure "Not implemented")
          | Pow ->
              if raise (Failure "Not implemented") then
		raise (Failure "Not implemented")
	      else raise (Failure "Not implemented")
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
        print_string "contains variable : ";
	print_string (string_of_bool (contains_var parsed));
	print_endline " ";
	print_string "Result of evaluation: ";
	print_float  (evaluate parsed xval);
	print_endline " ";
	print_string "Result of derivative: ";
	print_endline " ";
	print_string (to_string (derivative parsed));
	print_endline " ");;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  raise (Failure "Not implemented") ;;



(*>* Problem 2.5 *>*)

(* Karma problem:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  raise (Failure "Not implemented") ;;


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = raise (Failure "Not answered") ;;


