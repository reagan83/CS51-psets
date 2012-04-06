open Event
open World
open Pond
open Flower
open Hive
open Cave
open Pasture
open UI

let num_ponds = 12 
let pond_size = 15 
let gen_ponds () = 
  World.spawn_iter num_ponds pond_size 
                   (fun () -> ())
                   (fun p -> ignore (new pond p))

let num_flowers = 20
let flower_size = 20 
let gen_flowers () =
  let pollen_id = ref (-1) in
  World.spawn_iter num_flowers flower_size 
                   (fun () -> pollen_id := get_next_pollen_id ())
                   (fun p -> ignore (new flower p !pollen_id))

let test_bee () = 
  UI.run_world 
    (* Initializer *)
    begin fun () ->
      let hive = new hive (World.size/2,World.size/2) in
      ignore (new cave (0,0) hive) ;
      ignore (new pasture (World.size-1,World.size-1) hive) ;
      gen_ponds () ;
      gen_flowers () ;
    end
    (* Game Clock Action *)
    begin fun () ->
      Graphics.clear_graph () ; 
      Event.fire_event World.move_event () ;
      Event.fire_event World.action_event () ;
      Event.fire_event World.age_event () ;
      (* draw loop *)
      World.indices begin fun p -> 
        let sorted = List.sort (fun x y -> compare x#draw_z_axis y#draw_z_axis)
                               (World.get p)
        in
        List.iter (fun w -> w#draw) sorted
      end
    end
;;

test_bee () ;;
