(* Simple request-reply broker *)

open Zmq
open Helpers

let () =
    (* Prepare our context and sockets *)
    with_context @@ fun ctx ->

    with_socket ctx Socket.router @@ fun frontend ->
    with_socket ctx Socket.dealer @@ fun backend ->
    Socket.bind frontend "tcp://*:5559";
    Socket.bind backend "tcp://*:5560";

    (* Create a router-dealer proxy *)
    Proxy.create frontend backend;
