(*
Reading from multiple sockets
This version uses a simple recv loop
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

open Context
open Socket

let main () = 
  // Prepare our context and sockets
  use context = new Context(1)

  // Connect to task ventilator
  use receiver = context |> pull
  connect receiver "tcp://localhost:5557"

  // Connect to weather server
  use subscriber = context |> sub
  connect subscriber "tcp://localhost:5556"
  subscribe subscriber [ encode "10001" ]
  
  // Process messages from both sockets
  // We prioritize traffic from the task ventilator
  while true do
    // Process any waiting tasks
    match tryRecv receiver ZMQ.NOBLOCK with
    | Some(msg) -> msg |> decode |> printfn "%s" // Process task
    | None      -> () // Otherwise, do nothing
    // Process any waiting weather updates
    match tryRecv receiver ZMQ.NOBLOCK with
    | Some(msg) -> msg |> decode |> printfn "%s" // Process weather update
    | None      -> () // Otherwise, do nothing
    // No activity, so sleep for 1 msec
    sleep 1

  // We never get here
  EXIT_SUCCESS

main ()
