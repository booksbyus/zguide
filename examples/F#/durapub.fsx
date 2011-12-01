(*
  Publisher for durable subscriber
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // subscriber tells us when it's ready here
  use sync = pull context
  "tcp://*:5564" |> bind sync

  // we send updates via this socket
  use publisher = pub context
  "tcp://*:5565" |> bind publisher

  // wait for a synchronization request
  sync |> s_recv |> ignore

  // now broadcast exactly 10 updates with pause
  for update_nbr in 0 .. 9 do
    let message = sprintf "Update %d" update_nbr
    message |> s_send publisher
    sleep 1000
  "END" |> s_send publisher

  EXIT_SUCCESS
   
main ()
