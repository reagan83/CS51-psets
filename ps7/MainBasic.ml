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
      ignore (new pond (0,0)) ;
      ignore (new flower (1,1)) ;
      ignore (new hive (2,2)) ;
      ignore (new bee (3,3)) ;
      ignore (new cave (4,4)) ;
      ignore (new bear (5,5)) ;
      ignore (new pasture (6,6)) ;
      ignore (new cow (7,7)) ;
    end
    (* Game Clock Action *)
    begin fun () ->
      Graphics.clear_graph () ; 
      (* draw loop *)
      World.indices begin fun p -> 
        let sorted = List.sort (fun x y -> compare x#draw_z_axis y#draw_z_axis)
                               (World.get p)
        in
        List.iter (fun w -> w#draw) sorted
      end
    end
;;

run () ;;
