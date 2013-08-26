open WorldObject
open WorldObjectI
open Movable

(* ### Part 3 Actions ### *)
let pollen_theft_amount = 1000

(* ### Part 4 Aging ### *)
let bear_starting_life = 20

(* ### Part 2 Movement ### *)
let bear_inverse_speed = Some 10

class bear p hive: movable_t =
object (self)
  inherit movable p bear_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "bear"

  method draw = self#draw_circle (Graphics.rgb 170 130 110) Graphics.black ""

  method draw_z_axis = 3


  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = World.direction_from_to self#get_pos hive#get_pos


  (* ### TODO: Part 6 Custom Events ### *)

end
