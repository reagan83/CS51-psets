open WorldObject
open WorldObjectI

(** Random bees will move randomly. *)
class bee_random p hive : Bee.bee_t =
object (self)
  inherit Bee.bee p hive as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_random"

  (***********************)
  (***** Bee METHODS *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
   method private next_direction_default = Some(Direction.random World.rand)
end


