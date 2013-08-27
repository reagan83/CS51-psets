open WorldObject
open WorldObjectI
open Movable
open Hive

(* ### Part 3 Actions ### *)
let pollen_theft_amount = 1000

(* ### Part 4 Aging ### *)
let bear_starting_life = 20

(* ### Part 2 Movement ### *)
let bear_inverse_speed = Some 10

class bear p (hive:hive_i): movable_t =
object (self)
  inherit movable p bear_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable stolen_honey = 0

  (* ### TODO: Part 6 Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
     self#register_handler World.action_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method  private do_action () = 
     if self#get_pos = hive#get_pos
     then ignore(
       stolen_honey<-stolen_honey + 
         (hive#forfeit_honey pollen_theft_amount 
           (self:>WorldObjectI.world_object_i)))
     else ();

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "bear"

  method draw = self#draw_circle (Graphics.rgb 170 130 110) Graphics.black 
         (string_of_int stolen_honey)

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
