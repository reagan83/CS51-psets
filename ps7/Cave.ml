open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_bear_pollen = 500

(** A cave will spawn a bear when the hive has collected a certain amount of
    honey. *)
class cave p hive : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 6 Custom Events ### *)
  val mutable honey_count = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 6 Custom Events ### *)
  initializer
    (self#register_handler hive#get_pollen_event self#do_action);

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
   method private do_action () = 
    if (hive#get_pollen > spawn_bear_pollen)&&
       (World.fold (fun o b->o#get_name<>"bear"&&b) true) 
    then(
       ignore(Printf.printf "omg bears! ";flush_all());
       ignore(new Bear.bear(self#get_pos) hive (self:>world_object_i)))

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "cave"

  method draw = self#draw_circle (Graphics.black) Graphics.white "C"

  method draw_z_axis = 1


  (* ### TODO: Part 6 Custom Events *)
  method receive_pollen lst = 
    (honey_count<-honey_count+(List.length lst));[];
 
end
