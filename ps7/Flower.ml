open WorldObject
open WorldObjectI
open Helpers
(* ### Part 3 Actions ### *)
let next_pollen_id = ref 0
let get_next_pollen_id () =
  let p = !next_pollen_id in incr next_pollen_id ; p

(* ### Part 3 Actions ### *)
let max_pollen = 5
let produce_pollen_probability = 50
let bloom_probability = 4000
let forfeit_pollen_probability = 3

(* ### Part 4 Aging ### *)
let flower_lifetime = 2000

(** Flowers produce pollen.  They will also eventually die if they are not cross
    pollenated. *)
class flower p pollen_id: world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen_to_offer = World.rand max_pollen

  (***********************)
  (***** Initializer *****)
  (***********************)
    initializer
       self#register_handler World.action_event self#do_action

  (* ### TODO: Part 3 Actions ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)
                
  (* ### TODO: Part 3 Actions ### *)
    method private do_action () = 
      Helpers.with_inv_probability (World.rand) produce_pollen_probability 
        (fun()->if pollen_to_offer<max_pollen then
                    pollen_to_offer<-pollen_to_offer+1
                 else ());
      Helpers.with_inv_probability (World.rand) bloom_probability
         (fun()->
            (World.spawn 1 (self#get_pos) 
                (fun p ->ignore(new flower p pollen_id);()) );
            ()
         );

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "flower"

  (* ### TODO: Part 4 Aging ### *)
  method draw = self#draw_circle (Graphics.rgb 255 150 255) Graphics.black 
                        (string_of_int pollen_to_offer)

  method draw_z_axis = 1


  (* ### TODO: Part 3 Actions ### *)
  method smells_like_pollen = 
       if (pollen_to_offer > 0) then
         Some (pollen_to_offer)
       else 
         None

  method private bloom = World.spawn 1 self#get_pos 
  
  method forfeit_pollen =
     if pollen_to_offer>0 && (Random.int forfeit_pollen_probability)=0 then 
       (pollen_to_offer <- pollen_to_offer - 1; Some(pollen_id))
     else (None);

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

end
