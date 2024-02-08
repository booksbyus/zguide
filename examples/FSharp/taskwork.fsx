(*
Task worker
Connects PULL socket to tcp://localhost:5557
Collects workloads from ventilator via that socket
Connects PUSH socket to tcp://localhost:5558
Sends results to sink via that socket
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)
  
  // Socket to receive messages on
  use receiver = context |> Context.pull
  Socket.connect receiver "tcp://localhost:5557"

  // Socket to send messages to
  use sender = context |> Context.push
  Socket.connect sender "tcp://localhost:5558"

  // Process tasks forever
  while true do
    let msg = s_recv receiver
    // Simple progress indicator for the viewer
    fflush()
    printf "%s." msg

    // Do the work
    sleep (int msg)

    // Send results to sink
    s_send sender ""

  EXIT_SUCCESS
 
main ()
