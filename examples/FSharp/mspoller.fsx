(*
Reading from multiple sockets
This version uses zmq_poll()
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

open Context
open Socket

let main () = 
  use context = new Context(1)

  // Connect to task ventilator
  use receiver = context |> pull
  connect receiver "tcp://localhost:5557"

  // Connect to weather server
  use subscriber = context |> sub
  connect subscriber "tcp://localhost:5556"
  subscribe subscriber [ encode "10001" ]
  
  // Initialize pollset
  let items = 
    let printNextMessage = recv >> decode >> printfn "%s"
    [ Poll(ZMQ.POLLIN,receiver, fun s -> // Process task
                                         printNextMessage s)
      Poll(ZMQ.POLLIN,subscriber, fun s -> // Process weather update
                                           printNextMessage s) ]
  // Process messages from both sockets
  while true do
    (Polling.poll -1L items) |> ignore
  
  // We never get here
  EXIT_SUCCESS

main ()
