(*
  Simple message queuing broker
  Same as request-reply broker but using QUEUE device
*)
#r @"bin/fszmq.dll"
#r @"bin/fszmq.devices.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // socket facing clients
  use frontend = route context
  "tcp://*:5559" |> bind frontend

  // socket facing services
  use backend = deal context
  "tcp://*:5560" |> bind backend

  // start built-in device
  (frontend,backend) |> Devices.queue |> ignore

  // we never get here...
  EXIT_SUCCESS
   
main ()
