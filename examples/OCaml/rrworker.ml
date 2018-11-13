(**
 * Hello World worker
 * Connects REP socket to tcp://*:5560
 * Expects "Hello" from client, replies with "World"
 *)
open Zmq
open Helpers

let () =
    with_context @@ fun ctx ->

    (* Socket to talk to clients *)
    with_socket ctx Socket.rep @@ fun responder ->
    Socket.connect responder "tcp://localhost:5560";

    while true do
        (* Wait for next request from client *)
        let s = Socket.recv responder in
        printfn "Received request: [%S]" s;

        (* Do some 'work' *)
        sleep_ms 1;

        (* Send reply back to client *)
        Socket.send responder "World";
    done
