(**
  Weather update server
  Binds PUB socket to tcp://*:5556
  Publishes random weather updates
*)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  with_socket ctx Socket.pub @@ fun pub ->
  Socket.bind pub "tcp://*:5556";
  Socket.bind pub "ipc://weather.ipc";
  Random.self_init ();
  while not !should_exit do
    (* Get values that will fool the boss *)
    let zipcode = Random.int 100_000 in
    let temperature = Random.int 215 - 80 in
    let relhumidity = Random.int 50 + 10 in
    (* Send message to all subscribers *)
    Socket.send pub (Printf.sprintf "%05d %d %d" zipcode temperature relhumidity);
  done
