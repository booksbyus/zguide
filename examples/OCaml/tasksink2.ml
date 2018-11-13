(**
 * Task sink - design 2
 * Adds pub-sub flow to send kill signal to workers
 *)
open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  (* Socket to receive messages on *)
  with_socket ctx Socket.pull @@ fun receiver ->
  Socket.bind receiver "tcp://*:5558";

  (* Scoket for worker control *)
  with_socket ctx Socket.pub @@ fun controller ->
  Socket.bind controller "tcp://*:5559";

  (* Wait for start of batch *)
  let _ = Socket.recv receiver in

  (* Start our clock now *)
  let start_time = clock_ms () in

  (* Process 100 confirmations *)
  for taskNum = 0 to 99 do
    let _ = Socket.recv receiver in
    printfn @@ if ((taskNum / 10) * 10 == taskNum) then ":" else ".";
  done;

  printfn "Total elapsed time: %d msec" (clock_ms () - start_time);

  (* Send kill signal to workers *)
  Socket.send controller "KILL";
