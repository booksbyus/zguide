(*
Task sink
Binds PULL socket to tcp://localhost:5558
Collects results from workers via that socket
*)
#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  // Prepare our context and socket
  use context  = new Context(1)
  use receiver = context |> Context.pull
  Socket.bind receiver "tcp://*:5558"

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
  printfn "Total elapsed time: %d msec" (s_clock_stop watch)

  EXIT_SUCCESS

main ()
