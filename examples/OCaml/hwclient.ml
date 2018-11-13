(** Hello World client *)

open Zmq
open Helpers

let () =
  printfn "Connecting to hello world server...";
  with_context @@ fun ctx ->
  with_socket ctx Socket.req @@ fun req ->
  Socket.connect req "tcp://localhost:5555";
  for i = 0 to 9 do
    printfn "Sending Hello %d..." i;
    Socket.send req "Hello";
    let answer = Socket.recv req in
    printfn "Received %d : %S" i answer
  done
