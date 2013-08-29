open WorldObject
open WorldObjectI
open Movable
open Hive

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 100

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p (hive:hive_i) home : movable =
object (self)
  inherit movable p cow_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable num_of_eaten = 0

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
  method private do_action () = 
      (* home#get_pos = self#get_pos && not(self#hungry) then self#die
      else*)
      ignore(List.map 
        (fun x ->if x#smells_like_pollen <> None && self#hungry then
      x#die;Printf.printf "*nom* ";flush_all();num_of_eaten<-num_of_eaten+1) 
        (World.objects_within_range self#get_pos 0));
  
  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cow"

  method draw = self#draw_circle (Graphics.rgb 180 140 255) Graphics.black 
                      (string_of_int num_of_eaten)

  method draw_z_axis = 4


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = 
     if not(self#hungry) then(
        World.direction_from_to self#get_pos home#get_pos
     )
     else (
        if ((Random.int World.size)<=2) then 
            World.direction_from_to self#get_pos hive#get_pos
        else Some(Direction.random Random.int)
     )


  (* ### TODO: Part 6 Custom Events ### *)
  method private hungry = num_of_eaten < max_consumed_objects
end
