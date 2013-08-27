open Helpers
open WorldObject
open WorldObjectI
open Event

(** Class type for objects which constantly try to move in a calculated next
    direction. *)
class type movable_t =
object
  inherit world_object_i

  (** The next direction for which this object should move. *)
  method next_direction : Direction.direction option
end

class movable p (inv_speed:int option) : movable_t =
object (self)
  inherit world_object p as super

  (***********************)
  (***** Initializer *****)
  (***********************)
  initializer
     match inv_speed with
     | Some x -> self#register_handler (Event.buffer x World.move_event) 
         self#do_move
     | None -> ()

  (* ### TODO: Part 2 Movement ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)
  method private do_move (x:unit list) = self#move self#next_direction; ()

  (* ### TODO: Part 2 Movement ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  method next_direction = None

end
