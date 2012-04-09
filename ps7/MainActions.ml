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
                   (fun p -> ignore (new flower p))

let run () = 
  UI.run_world 
    (* Initializer *)
    begin fun () ->
      let hive = new hive (World.size/2,World.size/2) in
      ignore (new cave (0,0)) ;
      ignore (new pasture (World.size-1,World.size-1)) ;
      gen_ponds () ;
      gen_flowers () ;

      Array.iter begin fun _ ->
        ignore (new bee (World.size/2+1,World.size/2)) ;
      end (Array.make 20 ()) ;

      ignore (new bear (0,0) hive) ;
      ignore (new cow (World.size-1,World.size-1) hive)
    end
    (* Game Clock Action *)
    begin fun () ->
      Graphics.clear_graph () ; 
      (* draw loop *)
      Event.fire_event World.move_event () ;
      Event.fire_event World.action_event () ;
      World.indices begin fun p -> 
        let sorted = List.sort (fun x y -> compare x#draw_z_axis y#draw_z_axis)
                               (World.get p)
        in
        List.iter (fun w -> w#draw) sorted
      end
    end
;;

run () ;;
