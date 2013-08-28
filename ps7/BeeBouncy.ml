open WorldObject
open WorldObjectI

(** Bouncy bees will travel in a straight line in a random direction until an
    obstacle or edge of the world is reached, at which point a new random
    direction will be chosen. *)
class bee_bouncy p hive : Bee.bee_t =
object (self)
  inherit Bee.bee p hive as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Bees *)
  val mutable def_direction = None

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_bouncy"

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
 method private next_direction_default =
    let point_check optDir= 
     match optDir with
     |Some dir -> let result_point=Direction.move_point (self#get_pos) optDir in
        result_point <> self#get_pos && World.can_move result_point 
     |None -> false
    in
    let rec check_points (dirlist:Direction.direction list)=
      match dirlist with
      |hd::tl-> 
        if point_check (Some(hd)) then
           Some(hd)::check_points tl
        else check_points tl
      |[]->[None]
    in
    if not(point_check def_direction) then
      let lst = check_points Direction.all in
      (ignore(def_direction<-(List.nth lst (Random.int (List.length lst))));
             def_direction)
    else 
      def_direction
end


