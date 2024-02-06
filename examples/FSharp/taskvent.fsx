(*
Task ventilator
Binds PUSH socket to tcp://localhost:5557
Sends batch of tasks to workers via that socket
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)
  
  // Socket to send messages on
  use sender = context |> Context.push
  Socket.bind sender "tcp://*:5557"

  // Socket to send start of batch message on
  use sink = context |> Context.push
  Socket.connect sink "tcp://localhost:5558"

  printf "Press Enter when workers are ready: "
  scanln() |> ignore
  printfn "Sending tasks to workers"

  // The first message is "0" and signals start of batch
  s_send sink "0"

  // Initialize random number generator
  let rnd = srandom()

  // Send 100 tasks
  let mutable total_msec = 0
  for task_nbr in 0 .. 99 do
    // Random workload from 1 to 100 msecs
    let workload = (rnd.Next 100) + 1
    total_msec <- total_msec + workload
    s_send sender (string workload)
  
  printfn "Total expected cost: %d msec" total_msec
  sleep 1 // Give 0MQ time to deliver

  EXIT_SUCCESS

main ()
