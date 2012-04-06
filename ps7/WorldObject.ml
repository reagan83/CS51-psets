open WorldObjectI
open World
open Event
open Direction
open Draw

class type world_object_t =
object
  inherit world_object_i

  (** Register a handler with an event.  This handler will be removed if this
      object dies. *)
  method register_handler : 'a. 'a Event.event -> ('a -> unit) -> unit

  (** A helper function exported to subclasses of world_object.  Moves the
      object in the specified direction (None represents staying put). *)
  method move : Direction.direction option -> unit

  (** Draw a circle with background-color, foreground-color, and text at this
      object's location. See Draw.draw_circle for more info. *)
  method draw_circle : Graphics.color -> Graphics.color -> string -> unit

  (** Draw a status bar with color and full-amount where 0 <= full-amount <= 1.
      See Draw.status_bar for more info. *)
  method draw_status_bar : Graphics.color -> float -> unit
end

(** An abstract implementation of world_object_i that provides some helper
    functionality. 
    
    All world_object objects add themselves to the world at point initial_p upon
    creation. *)
class world_object (initial_p:int*int) : world_object_t =
object (self)

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val die_event : unit Event.event = Event.new_event ()
  val danger_event : world_object_i Event.event = Event.new_event () 

  val mutable pos : int*int = initial_p

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer 
    World.add pos (self :> world_object_i) ;

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private set_pos (p:int*int) : unit = 
    if World.can_move p then begin
      World.remove_must_exist pos (self :> world_object_i) ;
      World.add p (self :> world_object_i) ;
      pos <- p
    end else 
      ()

  (*******************************)
  (***** WorldObject Methods *****)
  (*******************************)

  method register_handler : 'a. 'a Event.event -> ('a -> unit) -> unit = 
    fun e f ->
      let id = Event.add_listener e f in
      ignore(Event.add_listener die_event (fun () -> Event.remove_listener e id))

  method move d =
    self#set_pos (Direction.move_point pos d)

  method draw_circle bg fg text =
    Draw.circle pos World.obj_width World.obj_height bg fg text

  method draw_status_bar c v =
    let h = World.obj_height / 10 in
    Draw.status_bar pos World.obj_width World.obj_height c h v

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "object"

  method get_pos = pos

  method draw = self#draw_circle Graphics.green Graphics.black ""

  method draw_z_axis = 1

  method is_obstacle = false

  method smells_like_pollen = None

  method forfeit_pollen = None

  method receive_pollen ps = ps

  method receive_sting = ()

  method get_die_event = die_event

  method die =
    Event.fire_event die_event () ;
    World.remove_must_exist pos (self :> world_object_i)

  method get_danger_event = danger_event

  method danger o = Event.fire_event danger_event o


end

