open WorldObject
open WorldObjectI
open Ageable
open CarbonBased
(* ### Part 2 Movement ### *)
let bee_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_pollen_types = 5

(* ### Part 4 Aging ### *)
let bee_lifetime = 1000

(* ### Part 5 Smart Bees ### *)
let max_sensing_range = 5

class type bee_t =
object 
  inherit Ageable.ageable_t
  
  method private next_direction_default : Direction.direction option
end 

(** Bees travel the world searching for honey.  They are able to sense flowers
    within close range, and they will return to the hive once they have
    pollenated enough species of flowers. *)
class bee p (home:world_object_i) : bee_t =
object (self)
  inherit carbon_based p bee_inverse_speed 
          (World.rand bee_lifetime) bee_lifetime as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollenlist = []
  (* ### TODO: Part 5 Smart Bees ### *)
  val sensing_range = World.rand max_sensing_range
  val pollen_types = World.rand max_pollen_types + 1
  (* ### TODO: Part 6 Custom Events ### *)
  val mutable target = None

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
     (self#register_handler World.action_event self#do_action);
     (self#register_handler home#get_danger_event self#danger_handler);
      
  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  method private danger_handler o =
     ignore(target<-Some o;
     self#register_handler o#get_die_event self#removetarget)
  
  method private removetarget () =
     ignore(target<-None);

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private deposit_pollen x =
   pollenlist<-x#receive_pollen pollenlist
  
  method private extract_pollen x =
   match x#forfeit_pollen with 
   |Some x -> ignore(pollenlist<-x::pollenlist); ()
   |None-> ()

  method private do_action () =
    let helper () = 
    ignore(
     List.map self#deposit_pollen (World.objects_within_range self#get_pos 0)); 
    ignore(
     List.map self#extract_pollen (World.objects_within_range self#get_pos 0));
    ()
    in
    match target with
    | Some x -> (if x#get_pos = self#get_pos then
                  ignore(x#receive_sting;self#die));helper ();
    | None -> helper();
   
 
  (* ### TODO: Part 5 Smart Bees ### *)
  method private magnet_flower :world_object_i option =
    let flowerlist = (List.filter (
     fun arg -> match arg#smells_like_pollen with 
     |Some x -> not(List.mem x pollenlist)
     |None -> false
   ) (World.objects_within_range self#get_pos sensing_range)) in
   if (List.length flowerlist > 0) then
     Some (List.nth flowerlist 0)
   else None
   

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "bee"

  (* ### TODO: Part 4 Aging ### *)
  method draw_picture = self#draw_circle (Graphics.yellow) Graphics.black (string_of_int (List.length pollenlist))

  method draw_z_axis = 2


  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method next_direction = 
    match target with
    | Some x -> World.direction_from_to self#get_pos x#get_pos
    | None ->
    if List.length pollenlist >= pollen_types then 
      World.direction_from_to self#get_pos home#get_pos
    else 
    match self#magnet_flower with
    |Some x -> World.direction_from_to self#get_pos x#get_pos
    |None -> self#next_direction_default

  (* ### TODO: Part 5 Smart Bees ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees ### *)
  method private next_direction_default = None
end
