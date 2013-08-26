open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let smelly_object_limit = 200

(** A pasture will spawn a cow when there are enough objects in the world that
    smell like pollen. *)
class pasture p : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method get_name = "pasture"

  method draw = self#draw_circle (Graphics.rgb 70 100 130) Graphics.white "P"

  method draw_z_axis = 1


end

