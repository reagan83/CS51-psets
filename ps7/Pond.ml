open WorldObject
open WorldObjectI

(** Ponds serve as obstruction for other world objects. *)
class pond p : world_object_i =
object (self)
  inherit world_object p as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
(*
  method get_name = raise TODO

  method draw = raise TODO

  method draw_z_axis = raise TODO

  method is_obstacle = raise TODO
*)

end
