(*****************************************************************************)
(*                           Part 1 - Warmup                                 *)
(*****************************************************************************)

(*>* Problem 1.0 *>*)
module type MATH =
sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
end

(* Write a module (struct) called Math that implements the MATH signature *)
module MATH =
struct
  let pi = 3.14
  let cos (x:float) = cos x
  let sin (x:float) = sin x
  let sum (x:float) (y:float) = x +. y
  let max (lst:float list) = 
     match lst with
     |[]->None
     |hd::tl-> Some (List.fold_left Pervasives.max hd tl)
end


(*>* Problem 1.1 *>*)

(*
 * Write a signature called LIST that only exposes the functions length,
 * fold_left, and rev. The Ocaml Documentation might be helpful
 *)


module type LIST =
sig
  val length : 'a list->int
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val rev : 'a list-> 'a list
end

(* The following should work after you have created your signature (you
 * can uncomment them *)


module MyList = (List : LIST);;

assert(MyList.length [1;2;3] = 3);;
assert(MyList.fold_left (+) 0 [1;2;3] = 6);;
assert(MyList.rev [1;2;3] = [3;2;1]);;



(* Even with your signature, the following line should never compile:

MyList.fold_right (+) [1;2;3] 0

*)


(*>* Problem 1.2 *>*)
module Kesha =
struct
    type album = Warrior | Animal

    let perform verse chorus bridge =
        print_string (verse ^ chorus ^ bridge ^ chorus)

    let verse = "Hot and dangerous\nIf you're one of us then roll with us\n"
    let bridge = "DJ turn it up\n"
    let chorus = "You know we're superstars\nWe are who we are\n"
    let rehearse () = perform verse chorus bridge

    let karaoke verse chorus bridge =
        let up = String.uppercase in
        perform (up verse) (up chorus) (up bridge)

    let doMath x y = y /. x
    let moreMath x = if x <= 0. then None else Some(log x)

    let album_num = fun a ->
      match a with
      | Warrior -> 2
      | Animal -> 1
end

module Adele =
struct
    let backup = "You're going to wish you\nNever had learned C\n"
    let chorus = "ML's the way to go, trolling in the heap\n"

    let perform chorus backup1 backup2 =
        raise (Failure "Sorry, my vocal chords are shot")

    let rec rehearse () = perform chorus backup backup
    let doMath x y = log x +. exp y
    let moreMath x = x ** 3.

    type album = Nineteen | TwentyOne

    let album_num album =
      match album with
      | Nineteen -> 1
      | TwentyOne -> 2
end

(* Write a signature SINGER that exposes as much from *both* Adele
 * and Kesha as possible. *)

module type SINGER =
sig
  type album
  val perform :string -> string -> string -> unit
  val chorus : string
  val rehearse : unit -> unit
  val doMath : float -> float -> float
  val album_num : album -> int
end


module SKesha = (Kesha : SINGER)
module SAdele = (Adele : SINGER)

