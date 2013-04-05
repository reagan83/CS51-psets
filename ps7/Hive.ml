open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let starting_pollen = 500
let cost_of_bee = 10
let spawn_probability = 20
let pollen_probability = 50
let max_pollen_deposit = 3

(** A hive will spawn bees and serve as a deposit point for the pollen that bees
    harvest.  It is possible to steal honey from a hive, however the hive will
    signal that it is in danger and its loyal bees will become angry. *)
class hive p : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)

  (* ### TODO: Part 5 Smart Bees ### *)

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)
(*
  method get_name = raise TODO

  method draw = raise TODO

  method draw_z_axis = raise TODO
*)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (************************)
  (***** Hive Methods *****)
  (************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

end
