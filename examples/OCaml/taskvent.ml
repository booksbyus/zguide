(**
  Task ventilator
  Binds PUSH socket to tcp://localhost:5557
  Sends batch of tasks to workers via that socket
*)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->

  (* Socket to send messages on *)
  with_socket ctx Socket.push @@ fun sender ->
  Socket.bind sender "tcp://*:5557";

  (* Socket to send start of batch message on *)
  with_socket ctx Socket.push @@ fun sink ->
  Socket.connect sink "tcp://localhost:5558";

  print_string "Press Enter when the workers are ready: ";
  flush stdout;
  let _ = input_line stdin in
  printfn "Sending tasks to workers...";

  (* The first message is "0" and signals start of batch *)
  Socket.send  sink "0";

  (* Initialize random number generator *)
  Random.self_init ();

  (* Send 100 tasks *)
  let total_msec = ref 0 in (* Total expected cost in msecs *)
  for i = 0 to pred 100 do
    (* Random workload from 1 to 100msecs *)
    let workload = Random.int 100 + 1 in
    total_msec := !total_msec + workload;
    Socket.send sender (string_of_int workload)
  done;
  printfn "Total expected cost: %d msec" !total_msec
