open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let next_pollen_id = ref 0
let get_next_pollen_id () =
  let p = !next_pollen_id in incr next_pollen_id ; p

(* ### Part 3 Actions ### *)
let max_pollen = 5
let produce_pollen_probability = 50
let bloom_probability = 4000
let forfeit_pollen_probability = 3

(* ### Part 4 Aging ### *)
let flower_lifetime = 2000

(** Flowers produce pollen.  They will also eventually die if they are not cross
    pollenated. *)
class flower p : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
(*
  method get_name = raise TODO

  (* ### TODO: Part 4 Aging ### *)
  method draw = raise TODO

  method draw_z_axis = raise TODO
*)

  (* ### TODO: Part 3 Actions ### *)


  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

end
