(**
 * Hello World client
 * Connects REQ socket to tcp://localhost:5559
 * Sends "Hello" to server, expects "World" back
 *)
open Zmq
open Helpers

let () =
    with_context @@ fun ctx ->

    (* Socket to talk to server *)
    with_socket ctx Socket.req @@ fun requester ->
    Socket.connect requester "tcp://localhost:5559";

    for requestNum = 0 to 9 do
        Socket.send requester "Hello";
        let s = Socket.recv requester in
        printfn "Received reply %d [%S]" requestNum s;
    done
