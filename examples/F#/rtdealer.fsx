(*
  Custom routing Router to Dealer

  While this example runs in a single process, that is just to make
  it easier to start and stop the example. Each thread has its own
  context and conceptually acts as a separate process.
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

open System.Threading

let worker_task_a () =
  use context = new Context(1)
  use worker  = deal context
  (ZMQ.IDENTITY,"A"B) |> set worker
  "tcp://localhost:5570" |> connect worker

  let rec loop count =
    let message = s_recv worker
    if  message = "END" 
      then count
      else loop (count + 1) 
  let total = loop 0
  fflush()
  printfn' "A received: %d" total

let worker_task_b () =
  use context = new Context(1)
  use worker  = deal context
  (ZMQ.IDENTITY,"B"B) |> set worker
  "tcp://localhost:5570" |> connect worker

  let rec loop count =
    let message = s_recv worker
    if  message = "END" 
      then count
      else loop (count + 1) 
  let total = loop 0
  fflush()
  printfn' "B received: %d" total
  
let main () = 
  use context = new Context(1)
  use client  = route context
  "tcp://*:5570" |> bind client

  let worker_a = Thread(ThreadStart worker_task_a)
  worker_a.Start()
  let worker_b = Thread(ThreadStart worker_task_b)
  worker_b.Start()

  // wait for threads to connect, 
  // since otherwise the messages we send won't be routable.
  sleep 100

  // send 10 tasks scattered to A twice as often as B
  let rand = srandom()
  for task_nbr in 0 .. 9 do
    // send two message parts, first the address...
    ( if rand.Next(0,3) > 0 
        then "A"B |~> client 
        else "B"B |~> client )
    // and then the workload
    <<| "This is the workload"B 
    
  ["A"B;"END"B] |> sendAll client
  ["B"B;"END"B] |> sendAll client

  EXIT_SUCCESS 
   
main ()
