(* Consider this mutable list type. *)
type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*>* Problem 1.1 *>*)
(* Write a function has_cycle that returns whether a mutable list has a cycle.
 * You may want a recursive helper function. Don't worry about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  let rec aux l v =
   print_string "aux";
   match l with
   | Nil -> false
   | Cons(h, { contents = t }) -> 
    if List.memq l v then 
      true
    else
      aux t (l::v)
  in
  aux lst []

(* Some mutable lists for testing. *)
let list1a = Cons(2, ref Nil)
let list1b = Cons(2, ref list1a)
let list1 = Cons(1, ref list1b)

let reflist = ref (Cons(2, ref Nil))
let list2 = Cons(1, ref (Cons (2, reflist)))
let _ = reflist := list2

let list2 = ref (Nil)
let list2a = Cons(1, list2)
let _ = list2 := list2a

let list3 = ref (Nil)
let list3b = (Cons(1, ref (Cons (2, ref (Cons (3, list3))))))
let _ = list3 := list3b;;

(*tests*)
assert(not(has_cycle(list1a)));;
assert(not(has_cycle(list1b)));;
assert(not(has_cycle(list1)));;
assert(has_cycle(list2a));;
assert(has_cycle(list3b));;

(*>* Problem 1.2 *>*)
(* Write a function flatten that flattens a list (removes its cycles if it
 * has any) destructively. Again, you may want a recursive helper function and
 * you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec aux l v =
     match l with
     | Nil -> ()
     | Cons(h, t) -> 
     if List.memq l v then 
       let _ = t := Nil in
       ()
     else
       aux !t (l::v)
  in
   aux lst []

(*>* Problem 1.3 *>*)
(* Use write mlength, which finds the number of nodes in a mutable list. *)
let mlength (lst : 'a mlist) : int =
  let rec aux l v i =
     match l with
     | Nil -> i
     | Cons(h, t) -> 
     if List.memq l v then 
       (i+1)
     else
       aux !t (l::v) (i+1)
  in
  aux lst [] 0

(*>* Problem 1.4 *>*)
(* Please give us an honest estimate of how long this part took 
 * you to complete.  We care about your responses and will use
 * them to help guide us in creating future assignments. *)
let minutes_spent : int = 65
