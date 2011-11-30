(*
  Broker peering simulation (part 2)
  Prototypes the request-reply flow

  While this example runs in a single process, that is just to make
  it easier to start and stop the example. Each thread has its own
  context and conceptually acts as a separate process.
*)
#r @"bin/fszmq.dll"
#r @"bin/fszmq.devices.dll"
open fszmq
open fszmq.Context
open fszmq.devices
open fszmq.Polling
open fszmq.Socket

#load "zhelpers.fs"

open System.Collections.Generic

let [<Literal>] NBR_CLIENTS = 10
let [<Literal>] NBR_WORKERS =  3

let LRU_READY = "\001"B

let client_task (o:obj) = 
  let fePort = o :?> int

  use ctx = new Context(1)
  use client = ctx |> req
  connect client (sprintf "tcp://localhost:%i" fePort)

  while true do
    // send request, get reply
    "HELLO"B |>> client
    client 
    |> recvAll 
    |> Array.last 
    |> dumpFrame (Some "Client: ")
    sleep 1

let worker_task (o:obj) =
  let bePort = o :?> int

  use ctx = new Context(1)
  use worker = ctx |> req
  connect worker (sprintf "tcp://localhost:%i" bePort)
  
  // tell broker we're ready for work
  LRU_READY |>> worker
  
  // process messages as they arrive
  while true do
    let msg = worker |> recvAll
    msg |> Array.last |> dumpFrame (Some "Worker: ")
    msg.[msg.Length - 1] <- "OK"B 
    msg |> sendAll worker

let main args =
  // first argument is this broker's name
  // other arguments are our peers' names
  match args |> Array.length with
  | argc when argc > 1 ->
      let self,peers = args.[1],if argc > 2 then args.[2..] else [||]
      printfn' "I: preparing broker at %s..." self

      let rand = srandom()
      let fePort,bePort = let port = int self in port + 1,port + 2
      
      // prepare our context and sockets
      use ctx = new Context(1)
      
      // bind cloud frontend to endpoint
      use cloudfe = ctx |> route
      (ZMQ.IDENTITY,encode self) |> set cloudfe
      bind cloudfe (sprintf "tcp://*:%s" self)

      // connect cloud backend to all peers
      use cloudbe = ctx |> route
      (ZMQ.IDENTITY,encode self) |> set cloudbe
      peers |> Array.iter (fun peer -> 
        printfn' "I: connecting to cloud frontend at '%s'" peer
        connect cloudbe (sprintf "tcp://localhost:%s" peer))

      // prepare local frontend and backend
      use localfe = ctx |> route
      bind localfe (sprintf "tcp://*:%i" fePort)
      use localbe = ctx |> route
      bind localbe (sprintf "tcp://*:%i" bePort)

      // get user to tell us when we can start…
      printf' "Press Enter when all brokers are started: "
      scanln() |> ignore

      // start local workers
      for _ in 1 .. NBR_WORKERS do (t_spawnp worker_task bePort) |> ignore
      
      // start local clients
      for _ in 1 .. NBR_CLIENTS do (t_spawnp client_task fePort) |> ignore

      (*  Interesting part
          -------------------------------------------------------------
          Request-reply flow
          - Poll backends and process local/cloud replies
          - While worker available, route localfe to local or cloud *)
      
      // queue of available workers
      let workers = Queue<byte array>()

      let msg = ref Array.empty<_>
      let reroutable = ref false
      
      let backends = 
        [ Poll(ZMQ.POLLIN,localbe,fun _ ->  
            // handle reply from local worker
            let reply = localbe |> recvAll
            reply.[0] |> workers.Enqueue
            // if it's READY, don't route the message any further
            msg := if reply.[2] = LRU_READY then [||] else reply.[2 ..]) 
          
          Poll(ZMQ.POLLIN,cloudbe,fun _ ->  
            // or handle reply from peer broker
            let frames = cloudbe |> recvAll
            // we don't use peer broker address for anything
            msg := frames.[2 ..]) ]
      
      let frontends = 
        [ Poll(ZMQ.POLLIN,cloudfe,fun _ ->
            msg := cloudfe |> recvAll
            reroutable := false) 
          
          Poll(ZMQ.POLLIN,localfe,fun _ ->
            msg := localfe |> recvAll
            reroutable := true) ]

      while true do
        let timeout = if workers.Count > 0 then 10000L else -1L 
        if backends |> poll timeout && (!msg).Length > 0 then 
          let address = (!msg).[0] |> decode
          // route reply to cloud if it's addressed to a broker
          if peers |> Array.exists (fun peer -> address = peer)
            then  !msg |> sendAll cloudfe
            else  // otherwise route reply to client
                  !msg |> sendAll localfe
          
        // Now route as many clients requests as we can handle
        while workers.Count > 0 && frontends |> poll 0L do
          // if reroutable, send to cloud 20% of the time
          // here we'd normally use cloud status information
          if !reroutable && peers.Length > 0 && rand.Next(0,5) = 0
            then  // route to random broker peer
                  let address = peers.[rand.Next peers.Length] |> encode
                  !msg 
                  |> Array.append [| address; Array.empty |]
                  |> sendAll cloudbe
            else  // route to local worker
                  let address = workers.Dequeue()
                  !msg 
                  |> Array.append [| address; Array.empty |] 
                  |> sendAll localbe
        // else ... No work, go back to backends

      EXIT_SUCCESS
  | _ -> 
      printfn "syntax: peering2 me {you}..."
      EXIT_FAILURE

main fsi.CommandLineArgs
