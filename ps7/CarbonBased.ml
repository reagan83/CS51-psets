open Ageable

(** Carbon based objects eventually die, and leave dust behind when they do. *)
class carbon_based p inv_speed starting_lifetime max_lifetime : ageable_t =
object (self)
  inherit ageable p inv_speed starting_lifetime max_lifetime as super

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging *)

end
