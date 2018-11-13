(**
  Task worker
  Connects PULL socket to tcp://localhost:5557
  Collects workloads from ventilator via that socket
  Connects PUSH socket to tcp://localhost:5558
  Sends results to sink via that socket
*)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  (* Socket to receive messages on *)
  with_socket ctx Socket.pull @@ fun receiver ->
  Socket.connect receiver "tcp://localhost:5557";

  (* Socket to send messages to *)
  with_socket ctx Socket.push @@ fun sender ->
  Socket.connect sender "tcp://localhost:5558";

  (* Process tasks forever *)
  while true do
    let s = Socket.recv receiver in
    printfn "%s." s;
    sleep_ms @@ int_of_string s; (* Do the work *)
    Socket.send sender ""; (* Send results to sink *)
  done
