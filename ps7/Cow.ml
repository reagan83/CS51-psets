open WorldObject
open WorldObjectI
open Movable

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 100

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p hive : movable =
object (self)
  inherit movable p cow_inverse_speed as super

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

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cow"

  method draw = self#draw_circle (Graphics.rgb 180 140 255) Graphics.black ""

  method draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = if ((Random.int World.size)<=2) then 
                               World.direction_from_to self#get_pos hive#get_pos
                          else Some(Direction.random Random.int)


  (* ### TODO: Part 6 Custom Events ### *)

end
