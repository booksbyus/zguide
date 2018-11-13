(**
  Task sink
  Binds PULL socket to tcp://localhost:5558
  Collects results from workers via that socket
*)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  with_socket ctx Socket.pull @@ fun receiver ->
  Socket.bind receiver "tcp://*:5558";

  (* Wait for start of batch *)
  let (_:string) = Socket.recv receiver in

  (* Start our clock now *)
  let start_time = clock_ms () in

  (* Process 100 confirmations *)
  for i = 0 to pred 100 do
    let (_:string) = Socket.recv receiver in
    print_char (if i mod 10 = 0 then ':' else '.');
    flush stdout;
  done;

  (* Calculate and report duration of batch *)
  printfn "Total elapsed time: %d msec" (clock_ms () - start_time)
