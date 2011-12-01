(*
  Multithreaded Hello World server
*)
#r @"bin/fszmq.dll"
#r @"bin/fszmq.devices.dll"
open fszmq
open fszmq.Context
open fszmq.Devices
open fszmq.Socket

#load "zhelpers.fs"

open System.Threading

let worker_routine (o:obj) =
  // socket to talk to dispatcher
  use receiver = (o :?> Context) |> rep
  "inproc://workers" |> connect receiver

  while true do
    let message = s_recv receiver
    printfn "Received request: [%s]" message
    // do some 'work'
    sleep 1
    "World" |> s_send receiver

let main () = 
  use context = new Context(1)

  // socket to talk to clients
  use clients = route context
  "tcp://*:5555" |> bind clients

  // socket to talk to clients
  use workers = deal context
  "inproc://workers" |> bind workers

  // launch pool of worker threads
  for thread_nbr in 0 .. 4 do
    let t = Thread(ParameterizedThreadStart worker_routine)
    t.Start(context)

  // connect work threads to client threads via a queue
  (clients,workers) |> queue |> ignore

  // we never get here but clen up anyhow
  EXIT_SUCCESS
   
main ()
