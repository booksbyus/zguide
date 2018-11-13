(** Hello World server *)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  with_socket ctx Socket.rep @@ fun resp ->
  Socket.bind resp "tcp://*:5555";
  while not !should_exit do
    let s = Socket.recv resp in
    printfn "Received : %S" s;
    sleep_ms 1000; (* Do some 'work' *)
    Socket.send resp "World";
  done
