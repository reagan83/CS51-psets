open Event
open World
open Pond
open Flower
open Hive
open Bee
open Cave
open Bear
open Pasture
open Cow
open UI

let run () = 
  UI.run_world 
    (* Initializer *)
    begin fun () ->
      ignore (new hive (World.size/2,World.size/2)) ;
      ignore (new bee (World.size/2+1,World.size/2)) ;
      ignore (new bear (0,0)) ;
      ignore (new cow (World.size-1,World.size-1)) ;
    end
    (* Game Clock Action *)
    begin fun () ->
      Graphics.clear_graph () ; 
      (* draw loop *)
      Event.fire_event World.move_event () ;
      World.indices begin fun p -> 
        let sorted = List.sort (fun x y -> compare x#draw_z_axis y#draw_z_axis)
                               (World.get p)
        in
        List.iter (fun w -> w#draw) sorted
      end
    end
;;

run () ;;
