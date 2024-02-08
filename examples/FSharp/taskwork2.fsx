(*
Task worker - design 2
Adds pub-sub flow to receive and respond to kill signal
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

open Context
open Socket
open Polling

let main () = 
  use context = new Context(1)
  
  // Socket to receive messages on
  use receiver = context |> pull
  connect receiver "tcp://localhost:5557"
    
  // Socket to send messages to
  use sender = context |> push
  connect sender "tcp://localhost:5558"

  // Socket for control input
  use controller = context |> sub
  connect controller "tcp://localhost:5559"
  subscribe controller [ ""B ]

  // Process messages from receiver and controller
  let doLoop = ref true
  let items =
    [ Poll(ZMQ.POLLIN,receiver,
           fun s -> let msg = s |> recv |> decode
                    // Do the work
                    sleep (int msg)
                    // Send results to sink
                    s_send sender ""
                    // Simple progress indicator for the viewer
                    fflush()
                    printf "%s." msg)
      Poll(ZMQ.POLLIN,controller,
           fun _ -> // Any waiting controller command acts as 'KILL')
                    doLoop := false) ]
    
  // Process messages from both sockets
  while !doLoop do (poll -1L items) |> ignore

  // Finished
  EXIT_SUCCESS
   
main ()
