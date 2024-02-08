(*
Task sink - design 2
Adds pub-sub flow to send kill signal to workers
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

open Context
open Socket
open Polling

let main () = 
  // Prepare our context and socket
  use context  = new Context(1)
  
  use receiver = context |> pull
  bind receiver "tcp://*:5558"

  // Socket for worker control
  use controller = context |> pub
  bind controller "tcp://*:5559"

  // Wait for start of batch
  s_recv receiver |> ignore

  // Start our clock now
  let watch = s_clock_start()

  // Process 100 confirmations
  for task_nbr in 0 .. 99 do
    s_recv receiver |> ignore
    printf (if (task_nbr / 10) * 10 = task_nbr then ":" else ".")
    fflush()

  // Calculate and report duration of batch
  printfn "\nTotal elapsed time: %d msec" (s_clock_stop watch)

  // Send kill signal to workers
  s_send controller "KILL"

  // Finished
  sleep 1   // Give 0MQ time to deliver
  EXIT_SUCCESS

main ()
