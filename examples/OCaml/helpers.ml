
open Zmq

let (@@) f x = f x
let (|>) x f = f x
let printfn fmt = Printf.ksprintf print_endline fmt

let finally fin f x = let r = try f x with exn -> fin (); raise exn in fin (); r
let bracket resource destroy k = finally (fun () -> destroy resource) k resource

let with_context k = bracket (Context.create ()) Context.terminate k
let with_socket ctx kind k = bracket Socket.(create ctx kind) Socket.close k

let clock_ms () = int_of_float @@ 1000. *. Unix.gettimeofday ()
let sleep_ms ms = let (_,_,_) = Unix.select [] [] [] (float_of_int ms /. 1000.) in ()

let should_exit = ref false
let () = Sys.(set_signal sigint (Signal_handle (fun _ -> should_exit := true)));
