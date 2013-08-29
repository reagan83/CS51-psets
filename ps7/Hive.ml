open WorldObject
open WorldObjectI
open Event
(* ### Part 3 Actions ### *)
let starting_pollen = 500
let cost_of_bee = 10
let spawn_probability = 20
let pollen_probability = 50
let max_pollen_deposit = 3

class type hive_i =
object 
  inherit world_object_i

  method get_pollen_event : unit Event.event
  
  method get_pollen : int 

  method forfeit_honey : int -> world_object_i -> int
end
	
(** A hive will spawn bees and serve as a deposit point for the pollen that bees
    harvest.  It is possible to steal honey from a hive, however the hive will
    signal that it is in danger and its loyal bees will become angry. *)
class hive p : hive_i =
object (self)
  inherit world_object p as super
  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen_count = starting_pollen

  (* ### TODO: Part 6 Custom Events ### *)
  val pollen_event = Event.new_event ()

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
     (self#register_handler World.action_event self#do_action);

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action () = 
        Helpers.with_inv_probability (World.rand) pollen_probability
               (fun () ->pollen_count<-pollen_count+1;()) ;
        Helpers.with_inv_probability (World.rand) spawn_probability
         (fun()->
            if pollen_count >= cost_of_bee then
              (pollen_count<-pollen_count-cost_of_bee;self#generate_bee;())
            else ()
         );()

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  method private generate_bee =
   (*if Rand.int 1 = 0 then*)
   World.spawn 1 (self#get_pos) 
   (fun p ->
     if Random.int 2 = 0 then
       (ignore(new BeeBouncy.bee_bouncy p (self:>world_object_i));())
     else  
       (ignore(new BeeRandom.bee_random p (self:>world_object_i));())
   );
   (*else ()*)

  (* ### TODO: Part 5 Smart Bees ### *)

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)

  method get_name = "hive"

  method draw = self#draw_circle (Graphics.cyan) Graphics.black 
                     (string_of_int pollen_count)

  method draw_z_axis = 1


  (* ### TODO: Part 3 Actions ### *)
  method forfeit_honey (n:int) (o:world_object_i) =
     self#danger o;
     if n > pollen_count then 
       let c = pollen_count in 
       pollen_count<-0; c
     else
       (pollen_count<-pollen_count-n; n)

   method receive_pollen lst = 
      ignore (Event.fire_event self#get_pollen_event ());
      pollen_count<-pollen_count+(min max_pollen_deposit (List.length lst));[]

  (* ### TODO: Part 6 Custom Events ### *)
   method get_pollen = pollen_count

  (************************)
  (***** Hive Methods *****)
  (************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)
   method get_pollen_event = pollen_event

end
