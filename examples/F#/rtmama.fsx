(*
  Custom routing Router to Mama (ROUTER to REQ)

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

let [<Literal>] NBR_WORKERS = 10

let rand = srandom()

let worker_task () =
  use context = new Context(1)
  use worker  = req context

  // we use a string identity for ease here
  s_setID worker
  "tcp://localhost:5571" |> connect worker

  let workerID = ZMQ.IDENTITY |> get worker |> decode
  let rec loop total =
    // tell the router we're ready for work
    "ready"B |>> worker

    // get workload from router, until finished
    let workload = s_recv worker
    if  workload = "END"
      then  printfn' "(%s) Processed: %d tasks" workerID total
      else  // do some random work
            sleep (rand.Next(0,1000) + 1)
            loop  (total + 1)
  loop 0

let main () = 
  use context = new Context(1)
  use client  = route context
  "tcp://*:5571" |> bind client

  for _ in 1 .. NBR_WORKERS do
    let worker = Thread(ThreadStart(worker_task))
    worker.Start()

  for _ in 1 .. (NBR_WORKERS * 10) do
    // LRU worker is next waiting in queue
    let address = recv client
    recv client |> ignore // empty
    recv client |> ignore // ready

    client <~| address 
           <~| ""B 
           <<| "This is the workload"B

  // now ask the mamas to shut down and report their results
  for _ in 1 .. NBR_WORKERS do
    let address = recv client
    recv client |> ignore // empty
    recv client |> ignore // ready
    
    client <~| address 
           <~| ""B 
           <<| "END"B

  EXIT_SUCCESS
   
main ()
