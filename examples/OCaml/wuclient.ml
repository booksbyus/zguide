(**
  Weather update client
  Connects SUB socket to tcp://localhost:5556
  Collects weather updates and finds avg temp in zipcode
*)

open Zmq
open Helpers

let () =
  with_context @@ fun ctx ->
  with_socket ctx Socket.sub @@ fun sub ->
  printfn "Collecting updates from weather server...";
  Socket.connect sub "tcp://localhost:5556";
  (* Subscribe to zipcode, default is NYC, 10001 *)
  let filter = match Array.to_list Sys.argv with _::zip::_ -> zip | _ -> "10001 " in
  Socket.subscribe sub filter;
  (* Process 100 updates *)
  let total_temp = ref 0 in
  for i = 0 to pred 100 do
    Scanf.sscanf (Socket.recv sub) "%d %d %d" begin fun _zipcode temperature _relhumidity ->
      total_temp := !total_temp + temperature
    end
  done;
  printfn "Average temperature for zipcode '%s' was %dF" filter (!total_temp / 100)
