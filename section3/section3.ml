(* CS51 Section 3 *)
(* Week of 2/20/12 *)

(* ******************** Part 1 - Modules ************************* *)
(* The purpose of this section is to get you comfortable with the
 * syntax and concepts behind signatures and modules. You will gain
 * new respect for the abstraction barrier and how different
 * implementation choices can matter - but shouldn't affect the way
 * the module is used.
 *)

(* Modules are useful for a variety of things. One of these is the
 * naming problem. A code base that implements both stacks and queues
 * requires distinct functions that logically do the same thing but on
 * different types.
 *)

(** Stack Implementation **)
type 'a stack = 'a list

let emp : 'a stack = [];;

let put (st : 'a stack) (v : 'a) : 'a stack = 
  ???
;;

let take (st : 'a stack) : ('a * 'a stack) option =
  ???
;;
  
(** Queue Implementation **)
type 'a queue = 'a list

let emp : 'a stack = [];;

let put (st : 'a queue) (v : 'a) : 'a queue =
  ???
;;

let take (st : 'a queue) : ('a * 'a queue) option =
  ???
;;

(* Since both implementations use the same function names we can't use them
 * both in the same code. We could solve this by renaming our functions,
 * for example change "put" to "stack_put" or "queue_put", but this is ugly
 * when we are only using one.
 * 
 * We can solve this problem using modules. The primary motivation for modules
 * is to package together related definitions and enforce a consistent naming
 * scheme for these definitions. They wrap a set of definitions into their 
 * own container or "namespace".
 *)

(** Stack Implementation **)
module Stack =
struct
  type 'a t = ???

  let emp : 'a t = ???

  let put (st : 'a t) (v : 'a) : 'a t =
   ??? 

  let take (st : 'a t) : ('a * 'a t) option =
   ???
end;;
  
(** Queue Implementation **)
module Queue =
struct
  type 'a t = ???
  
  let emp : 'a t = ???

  let put (st : 'a t) (v : 'a) : 'a t =
    ???
  
  let take (st : 'a t) : ('a * 'a t) option =
   ???
end;;

(* In OCaml, by convention, types are usually given the name "t" so we've
 * renamed the stack and queue types to "t".
 * 
 * We can refer to values in the module using "dot-notation".
 *)

(** The empty queue **)
Queue.emp;;

(** The empty stack **)
Stack.emp;;

(** Adding values to a queue **)
Queue.put Queue.emp 5;;

(* We've successfully de-cluttered our namespace by factoring the code into
 * two modules, but the modules aren't providing us with anything beyond
 * this. For example, both stacks and queues are implemented using lists but
 * our modules are not hiding these facts. This means that OCaml will accept
 * bogus programs such as the following:
 *)

(* THE FOLLOWING IS VERY BAD *) 

Queue.take (Stack.put Queue.emp 5);;

Queue.emp = Stack.emp;;

(* ******************** Part 2 - Signatures ************************* *)
(* To check these programs, OCaml is "unfolding" the type "'a t" and replacing
 * it with "'a list". This breaks abstraction because we might have invariants
 * on certain data structures that are not expressible in the OCaml type system.
 * For example, that a list should be sorted or should never be empty.
 * 
 * One way to address this problem is to treat the type like a variable telling
 * OCaml, essentially:
 * 
 *   There's this type "'a t", but I'm not going to tell you anything else
 *   about it.
 * 
 * To achieve this in OCaml we use module signatures, which are analagous to
 * types in that, in classic SAT style:
 * 
 *       'values' are to 'types' as
 *       'modules' are to 'signatures'
 * 
 * We define signatures with the following OCaml syntax:
 *)

module type SCHEDULER =
sig
  type 'a t
  val emp : 'a t
  val put : 'a t -> 'a -> 'a t
  val take : 'a t -> ('a * 'a t) option
end
;;

(* There are several things to note about this declaration in comparison to
 * modules that we defined above.
 * 1) We omit the definition of the type, i.e. the " = 'a list", so that OCaml
 *    must reason about that type abstractly.
 * 2) "let" declarations are changed to "val" declarations and only contain
 *    types. 
 * 
 * In the same way that we can assert types for values in OCaml, we can use the
 * ":" operator to assert that a module satisfies a signature.
 *)

(** Stack Implementation (is an implementation of the SCHEDULER signature) **)

module SStack: SCHEDULER =
struct
  type 'a t = ???
  let emp: 'a t = ???
  let put (st: 'a t) (v: 'a) : 'a t = ???
  let take (st: 'a t) : ('a * 'a t) option = ???
end

(* Or, you can just say: *)
module SStack = (Stack : SCHEDULER)

(** Queue Implementation (is an implementation of the SCHEDULER signature) **)

module SQueue: SCHEDULER = 
struct
  type 'a t = ???
  let emp: 'a t = ???
  let put (st: 'a t) : (v: 'a) : 'a t = ???
  let take (st: 'a t) : ('a * 'a t) option = ???
end

(* Or you can just say: *)
module SQueue = (Queue: SCHEDULER) 

(* Now that we have "sealed" the module, OCaml will treat the type "t"
 * abstractly. This prevents us from incorrectly using values of type "Stack.t"
 * in the Queue module and values of type "Queue.t" in the Stack module. 
 *)

(* Stack.put Queue.emp 5;; *)
(* (\*> Error: This expression has type 'a Queue.t *)
(*  *>        but an expression was expected of type 'b Stack.t *)
(*  *\) *)

(* Queue.emp = Stack.emp;; *)
(* (\*> Error: This expression has type 'a Stack.t *)
(*  *>        but an expression was expected of type 'b Queue.t *)
(*  *\) *)

(* ******************** Part 3 - Functors ************************* *)
(* Signatures allow us to describe a module without needing to give a
 * precise "instantiation" of the signature. Continuing with the type
 * analogy, this is similar to our ability to describe all integers by
 * the type "int". This ability allows us to write functions over
 * modules. For more mathematical reasons, functions over modules are
 * called functors.
 *)

(* We're going to look at writing a simple functor that allows us to
 * traverse trees using a standard "work-list" algorithm. First we'll
 * consider the algorithm looking at some concrete instances.
 * 
 *    [0]
 *   /  \
 * [1]  [2]
 * 
 * The algorithm proceeds as follows:
 * 
 *  visit node _n_:
 *    + add each child of n to the work-list
 *    + get the next node _m_ to visit from the work-list
 *    + visit _m_
 * 
 *)
type tree = 
  | Leaf
  | Branch of tree * int * tree
;;

let traverse_df (tr : tree) : unit =
  let rec traverse_help (work_list : tree list) : unit =
    match work_list with
      | [] -> ()
      | Leaf :: rest -> traverse_help rest
      | Branch (l,v,r) :: rest -> 
	let _ = print_string (string_of_int v) in
	traverse_help (l :: r :: rest)
  in
  traverse_help [tr]
;;

let traverse_bf (tr : tree) : unit =
  ???
;;

(* We can treat the work list abstractly. We don't care how it is implemented.
 * Therefore we can use a functor to abstract over the QUEUE implementation.
 *)
module Traverse =
  functor (Q : SCHEDULER) ->
struct
  let traverse (tr : tree) : unit =
    let rec traverse_help (work_list : tree Q.t) : unit =
      match ??? with
	| ??? -> ()
	| ??? (Leaf,rest) -> traverse_help rest
	| ??? (Branch (l,v,r), rest) ->
	  let _ = print_string (string_of_int v) in
	  traverse_help (List.fold_left (???) rest [l;r])
    in
    traverse_help ???
end
;;

(* We can apply the functor to any module that has the signature QUEUE.
 * When we apply it to the stack we get a depth-first traversal.
 *)
module DepthFirstTraversal = Traverse (Stack);;

(* We can also apply the functor to the Queue, since it also implements
 * the QUEUE signature. What kind of traversal does this give us?
 *)
module ??? = Traverse (Queue);;

(* ***** Part 4 - Modules and Functors in the Standard Library **** *)
(* Now that we have seen how to write our own modules and functors we're
 * going to look at the conventions used for modules and functors in the
 * OCaml standard library.
 * 
 * The OCaml standard library index can be found online at:
 *   http://caml.inria.fr/pub/docs/manual-ocaml/manual034.html
 * 
 * The standard library provides a several data structures that are
 * built generically using modules and functors. For example, it 
 * provides modules/functors for: finite sets, finite maps, stacks, 
 * queues.
 * 
 * As a representative example, we're going to look at the set module.
 * Documentation:
 *   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.html
 *)

(* Declare a finite set module over integers using the standard library's,
 * Set.Make functor.
 *)
module IntCompare : Set.OrderedType with type t = int  =
struct
  type t = int
  let compare = compare
end;;

module IntSet = Set.Make (IntCompare);;

(* Now use your IntSet module to find the number of unique elements in a
 * list. Hint: Write a helper function that converts a list into an IntSet.
 *)
let set_of_list (ls : int list) : IntSet.t =
  (* ??? *)
;;

let count_unique (ls : int list) : int = 
  (* ??? *)
;;

(* Now use your function to sum the unique elements in a list.
 *)
let sum_unique (ls : int list) : int = 
  (* ??? *)
;;

(* ******************** Part 5 - Binary Heaps ******************* *)
(* PS4 requires implementing a binary heap. Answer the following questions.
 *
 * What is a binary heap?  
 *
 * Why do we care about binary heaps?
 *
 * What is an ODD tree? What is an EVEN tree?
 *
 * What does it mean for a tree to be balanced?
 *
 * What is the representation invariant for a binary heap?
 *
 * Binary heaps are defined recursively, however they must respect a strict
 * invariant. How can we write recursive functions over binary heaps?
 *
 * How would you design the `mirror' function which swaps the left branch with
 * the right for a binary heap?
 *)

(* ******************** Part 6 - More Modules ************************* *)
(* We're now going to look at some more modules for implementing abstract 
 * data types to get more comfortable with them.
 * 
 * First consider the following signature for arbitrary sized integers:
 *)

module type BIGNAT =
sig
  type t
  val zero : t
  val is_zero : t -> bool
   
  (* add 1 *)
  val succ : t -> t
  (* minus 1 *)
  val pred : t -> t

  val show : t -> string
end;;

(* One simple, though very inefficient, implementation of big integers is
 * natural numbers. Natural numbers can be defined inductively as:
 * 
 *   A natural number is either:
 *     0 - "zero"
 *   -or-
 *     S n - "successor of n" (i.e. n + 1)
 * 
 * Implement the Nat module using this inductive definition.
 *)

module Nat : BIGNAT =
struct
  type t = Z | S of t
      
  let zero : t = ???

  let is_zero (v : t) = ???

  let succ (v : t) : t = ???
    
  let pred (v : t) : t = ???
                
                 
                

  let show (v : t) : string =
    let rec to_int (v : t) (a : int) : int =
      match v with
      | Z -> a
      | S v -> to_int v (a + 1)
    in
    string_of_int (to_int v 0)
end;;
