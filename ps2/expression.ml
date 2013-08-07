
open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 * 3. Remember to test using the function "assert".
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
    match e with
    |Unop (u, e) ->    contains_var e
    |Binop (u, e, f) -> contains_var e||contains_var f
    |Var -> true
    |Num n-> false
;;

(* tests *)
assert(contains_var (parse "x^4") = true);;
assert(contains_var (parse "4+3") = false);;

(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
    match e with
    |Unop (u, e) -> (match u with
                    |Sin -> sin (evaluate e x)
		    |Cos -> cos (evaluate e x)
		    |Ln  -> log (evaluate e x)
		    |Neg -> -. evaluate e x)
    |Binop (u, e, f) -> (match u with
                        |Add -> evaluate e x +. evaluate f x
			|Sub -> evaluate e x -. evaluate f x
			|Mul -> evaluate e x *. evaluate f x
			|Div -> evaluate e x /. evaluate f x
			|Pow -> evaluate e x ** evaluate f x)
    |Var -> x
    |Num n -> n
;;

(* tests *)
assert(evaluate (parse "x^4 + 3") 2.0 = 19.0);;
assert(evaluate (parse "12 + x*2") 3.0 = 18.0);;


(*>* Problem 2.3 *>*)

(* See writeup for instructions. We have pictures! *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Div,derivative e1, e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div,Binop(Sub,
                               Binop(Mul,derivative e1,e2),
                               Binop(Mul,e1,derivative e2)),
                           Binop(Pow,e2,Num(2.0)))
        | Pow ->
            if contains_var e2
            then Binop(Mul,Binop(Pow,e1,e2),
                           Binop(Add,Binop(Mul,derivative e2, Unop(Ln, e1)),
                                     Binop(Div,Binop(Mul,derivative e1,e2),e1)))
            else Binop(Mul,Binop(Mul,e2,derivative e1),
                           Binop(Pow,e1,Binop(Sub,e1,Num 1.)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;

(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  let rec aux (count,f)= 
   if count > lim then 
     None
   else if abs_float (evaluate e f) < epsilon then 
     Some f
   else
     aux (count+1, (f -. evaluate e f/. evaluate (derivative e) f))
  in
  aux (0, g)
;;

(* tests seem to work, except powers (power of, exponents) *)
(*
let myExp = Binop(Sub,Binop(Pow,Var,Num(2.)),Num (9.)) in
(*find_zero myExp 51. 100. 100;;*)
*)

let myExp = Binop(Sub,Var, Num(9.)) in
print_float(match find_zero myExp 53. 4. 100 with |Some c->c|None->0.);;


(*>* Problem 2.5 *>*)

(* Karma problem:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
    raise (Failure "Not implemented")
;;


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 35;;
