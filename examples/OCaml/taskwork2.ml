(**
 * Task worker - design 2
 * Adds pub-sub flow to receive and respond to kill signal
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

  (* Socket for control input *)
  with_socket ctx Socket.sub @@ fun controller ->
  Socket.connect controller "tcp://localhost:5559";
  Socket.subscribe controller "";

  let items = Poll.mask_of [| (receiver, Poll.In); (controller, Poll.In) |] in

  (* Process tasks from either socket *)
  while true do
    let pollResults = Poll.poll items in
    let receiverEvents = pollResults.(0) in
    let ctrlEvents = pollResults.(1) in

    match receiverEvents with
    | Some _ ->
      let s = Socket.recv receiver in
      printfn "%S." s; (* Show progress *)
      sleep_ms (int_of_string s); (* Do the work *)
      Socket.send sender ""; (* Send results to sink *)
    | _ -> ();

    (* Any waiting controller command acts as 'KILL' *)
    match ctrlEvents with
    | Some _ -> exit 0; (* Exit program *)
    | _ -> ();
  done
