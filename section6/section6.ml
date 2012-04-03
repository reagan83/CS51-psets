(* CS51 Spring 2012
Section 6 Notes
Week of 3/26/12 *)

(******************  Part 1: Mutation and State  ******************)
(* So far in this course, we have worked almost exclusively with the
 * subset of ML that is functional (with the exceptions of exceptions
 * (sorry) and printing).  We have traded iteration counters for
 * recursion, among other things.
 *
 * Recall from lecture that, while there are plenty of
 * reasons to sing the praises of functional programming, eventually
 * we need side-effects.  After all, we write programs in order to
 * change the world, and any change to the world is necessarily a side-effect
 * of functional computation.
 *
 * The basic way of allowing for mutable state in ML is to use references.
 * To recap from lecture:
 *
 * New type: t ref
 * ->Think of it as a pointer to a box that holds a t value.
 * -> The pointer can be shared.
 * -> The contents of the box can be read or written.
 *
 * To create a fresh box: ref 42
 * -> allocates a new box, initializes its contents to 42, and returns a
 *    pointer to that box.
 * To read the contents: !r
 * -> If r points to a box containing 42, then returns 42.
 * -> similar to *r in C/C++
 *
 * To write the contents: r := 42
 * -> updates the box that r points to so that it contains 42.
 * -> similar to *r = 42 in C/C++
 *
 * Note that:
 * Now that we are updating state, sometimes we will want to have a sequence
 * of expressions, but we will only care about the value of the last
 * expression.  We accomplish this with semicolons.
 * For example, (a := 5 + b; !a) would update the "box" that a points to
 * to have a value of 5 more than the value of b, and then evaluate
 * to this same value.
 *
 *)

(* 1.1: Vocabulary Check
 * 
 * What is the name for something that becomes a problem in the presense of 
 * references and mutation, defined by Wikipedia as "situation in which
 * a data location in memory can be accessed through different symbolic
 * names in the program"? 
 *)


(* 1.2: 42 Exercises *)
(* Replace ??? so that the expression evaluates to 42. *)
(* 1.2.1 *)
let f = (fun () -> ??? ) in             
  match f () with          
    | [] -> 12
    | a::b -> !a 21
;;

(* 1.2.2 *)
let f = ??? in
  if f () = 42 then 21 else f ()
;;

(* 1.3: Call Counter
 * Write call_counter : ('a -> 'b) -> (('a->'b)*int ref)
 * The second component of the returned pair should
 * contain the number of times the first component has
 * been called. *)
let call_counter (f:'a -> 'b) : (('a->'b)*int ref) =
  

;;

(* 1.4: Aliasing
 * What does x evaluate to?
 *)

let x =
  let r = ref 1 in
  let rs = [r;r;r] in
  List.map (fun x -> x := (!x) + 1; !x) rs 

(************************* Part 2: Streams ***************************)
(* Recall our definition of the stream datatype, and some
 * basic operations we defined on streams. *)

type 'a str = Cons of 'a * ('a stream)
and 'a stream = unit -> 'a str

let head(s:'a stream):'a =
  let Cons(h,_) = s () in h
;;

let tail(s:'a stream):'a stream =
  let Cons(_,t) = s () in t
;;

let rec map(f:'a->'b)(s:'a stream):'b stream =
  fun () -> Cons(f (head s), map f (tail s))
;;

let rec filter p s =
  if p (head s) then
    (fun () -> (Cons (head s, filter p (tail s))))
  else (filter p (tail s))
;;

(* 2.1 *)
(* What happens if p doesn't match anything in the stream? *)


(* Here are some useful streams: *)
let rec ones = 
  (fun () -> Cons(1, ones))
;;

let rec nats =
  (fun () -> Cons(0, map (fun x -> x + 1) nats))
;;

(* 2.2 *)
(* Define a stream that contains all integer multiples of 3. *)

let mult3 =
	
;;

(* 2.3 *)
(* Write a function that takes an integer n and a stream
 * and returns a list containing the first n elements of the stream. *)
let rec first (n: int) (s: 'a stream) : 'a list =


;;

(* 2.4.1 *)
(* Define a function alternate that takes the negative of every other
 * item in a stream, starting with the second. e.g., 1,1,1,1... would
 * become 1,-1,1,-1,... *)
let rec alternate (s: float stream) : float stream =

;;

(* 2.4.2 *)
(* Another way to write alternate, without a using map? *)
let rec alternate' (s: float stream) : float stream =




;;

(* 2.5 *)
(* Define a stream that contains the alternating harmonic sequence, whose
 * nth term is (-1)^(n+1)(1/n), e.g., 1, -1/2, 1/3, -1/4... 
 * You may use nats. *)
let altharm : float stream =

;;

(* 2.6 *)
(* Write a function streammax that takes two streams and returns 
a new stream of the maximum of the first elements of s1 and s2, followed 
by the maximum of the second element of each, and so on. (For example, 
s1 and s2 might represent simultaneous rolls of two dice, and we want a 
stream representing the maximum in each pair of rolls.) *)

let rec streammax (s1: int stream) (s2: int stream) : int stream =



;;

(* 2.7 - Collatz Conjecture *)
(* Consider the following procedure:
 * Take an integer. If the integer is odd, multiply by 3 and add 1.
 * If it is even, divide it by 2. Repeat this procedure.
 * The Collatz Conjecture states that if this procedure is repeated, starting
 * with any positive integer, it will eventually reach 1.
 *
 * For more information, see this scholarly article:
 * http://en.wikipedia.org/wiki/Collatz_conjecture, or this scholarly
 * webcomic: http://www.xkcd.com/710/. *)

(* 2.7.1 *)
(* Write a function, collatz, that takes an integer and returns an int
 * stream with the integers that result from performing the above process.
 * Since streams must be infinite according to our definition, once (or if)
 * 1 is reached, the rest of the stream should be ones. *)

let even x = (x mod 2 = 0);;

let rec collatz (n: int) : int stream =






;;

(* 2.7.2 *)
(* We can define a stream of streams with the collatz streams starting
 * with each natural number. *)

let collatzes = map collatz nats;;

(* And a predicate that determines if a stream contains 1. *)
let rec hasone s =
  (head s = 1) || (hasone (tail s))
;;

(* Now consider the following definition: 
 *
 * let collatzproof = filter (fun s -> not (hasone s)) collatzes;;
 *
 * collatzproof is then a stream of streams which contains any collatz stream
 * that does not contain the number 1. If the stream collatzproof has an 
 * element, the Collatz conjecture is false. If it is empty, the conjecture
 * is true. *)

(* Why can't we use this to prove or disprove the Collatz conjecture? *)




(************************* Part 3: Memoization ***************************)
(* Every time we take elements of a stream, they must be recomputed. This
 * results in extra processing time. A solution to this is memoization. *)

(* Exercise 3.1 *)
(* Consider the following stream. *)
let rec randstream = 
  (fun () -> let random = Random.int(100) in Cons(random, randstream));;
let a = head randstream;;
let b = head randstream;;
(* Are a and b necessarily equal? Why or why not? What if we use
 * memoized streams? *)






(* Exercise 3.2 *)
(* What's the tradeoff? When might we might not want to memoize? *)
