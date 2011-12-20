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
let [<Literal>] NBR_WORKERS =  5

let LRU_READY = [| 1uy |] // signals worker is ready

let rand = srandom()

// request-reply client using REQ socket
// to simulate load, clients issue a burst of requests 
// and then sleep for a random period.
let client_task (o:obj) = 
  let frontPort,monitorPort = o :?> (int * int)

  use ctx = new Context(1)
  use client = ctx |> req
  connect client (sprintf "tcp://localhost:%i" frontPort) 
  use monitor = ctx |> push
  connect monitor (sprintf "tcp://localhost:%i" monitorPort) 

  let pollset socket taskID = 
    [Poll(ZMQ.POLLIN,socket,fun s -> 
          let reply = recv s
          // worker is supposed to answer us with our task ID
          reply |> dumpFrame (Some "Client: ")
          assert ((decode reply) = taskID))]

  let rec burst = function
    | n when n > 0 ->
        let taskID = sprintf "%04X" (rand.Next 0x10000)
        
        // send request with random hex ID
        taskID |> encode |>> client

        match (taskID |> pollset client) |> poll 100000L with
        | true  -> burst (n - 1)
        | _     -> false,taskID

    | _ -> true,"<none>"

  let rec loop () =
    sleep ((rand.Next 5) * 1000)
    match burst (rand.Next 15) with
    | false,taskID  ->  (sprintf "E: CLIENT EXIT - lost task %s" taskID)
                        |> encode
                        |>> monitor
    | _             ->  loop()

  loop()

// worker using REQ socket to do LRU routing
let worker_task (o:obj) = 
  let backPort = o :?> int
  
  use ctx = new Context(1)
  use worker = ctx |> req
  connect worker (sprintf "tcp://localhost:%i" backPort)

  // tell broker we're ready for work
  LRU_READY |>> worker

  while true do
    // workers are busy for 0/1/2 seconds
    let msg = recvAll worker
    sleep ((rand.Next 3) * 1000)
    msg |> sendAll worker

let main args =
  // first argument is this broker's name
  // other arguments are our peers' names
  match args |> Array.length with
  | argc when argc > 1 ->
      let cloudPort,self = let n = args.[1] in (int n),(encode n)
      let peers = if argc > 2 then args.[2..] else [||]
      let isPeer address = peers |> Array.exists ((=) address)
      let frontPort,backPort,statePort,monitorPort =  cloudPort + 1,
                                                      cloudPort + 2,
                                                      cloudPort + 3,
                                                      cloudPort + 4
      //NOTE: to run this example on Windows, we must use tcp... 
      //      so when we do, assume inputs are port numbers, and we use 
      //      them as the basis for additional (internal to the cluster)
      //      port numbers on non-windows systems, we can use ipc (as per 
      //      the guide) so in *that* case, inputs are alphanumeric IDs
      printfn' "I: preparing broker at %i..." cloudPort

      // prepare our context and sockets
      use ctx = new Context(1)

      // bind cloud frontend to endpoint
      use cloudfe = ctx |> route
      (ZMQ.IDENTITY,self) |> set cloudfe
      bind cloudfe (sprintf "tcp://*:%i" cloudPort)

      // bind state backend / publisher to endpoint
      use statebe = ctx |> pub
      bind statebe (sprintf "tcp://*:%i" statePort)

      // connect cloud backend to all peers
      use cloudbe = ctx |> route
      (ZMQ.IDENTITY,self) |> set cloudbe
      peers |> Array.iter (fun peer -> 
        printfn' "I: connecting to cloud frontend at '%s'" peer
        connect cloudbe (sprintf "tcp://localhost:%s" peer))

      // connect statefe to all peers
      use statefe = ctx |> sub
      [""B] |> subscribe statefe
      peers |> Array.iter (fun peer -> 
        let peerPort = (int peer) + 3
        printfn' "I: connecting to state backend at '%i'" peerPort
        connect statefe (sprintf "tcp://localhost:%i" peerPort))

      // prepare local frontend and backend
      use localfe = ctx |> route
      bind localfe (sprintf "tcp://*:%i" frontPort)

      use localbe = ctx |> route
      bind localbe (sprintf "tcp://*:%i" backPort)

      // prepare monitor socket
      use monitor = ctx |> pull
      bind monitor (sprintf "tcp://*:%i" monitorPort)

      // start local workers
      for _ in 1 .. NBR_WORKERS do 
        ignore (t_spawnp worker_task backPort)

      // start local clients
      for _ in 1 .. NBR_CLIENTS do 
        ignore (t_spawnp client_task (frontPort,monitorPort))

      (*  Interesting part
          -------------------------------------------------------------
          Publish-subscribe flow
          - Poll statefe and process capacity updates
          - Each time capacity changes, broadcast new value
          Request-reply flow
          - Poll primary and process local/cloud replies
          - While worker available, route localfe to local or cloud *)

      // queue of available workers
      let workers = Queue()

      let rec secondary localCapacity cloudCapacity =

        if localCapacity + cloudCapacity > 0 then
      
          let message = ref None
          let fetchMessage socket = message := Some(recvAll socket)
      
          let pollset =
            [ yield Poll( ZMQ.POLLIN,localfe,fetchMessage )
              if workers.Count > 0 then 
                yield Poll( ZMQ.POLLIN,cloudfe,fetchMessage )]
      
          if pollset |> poll 0L then
            !message |> Option.iter (fun msg ->
              let address,backend = 
                match localCapacity with
                | 0 ->  // route to random broker peer
                        encode peers.[rand.Next peers.Length],cloudbe
                | _ ->  // route to local worker
                        workers.Dequeue(),localbe
              msg 
              |> Array.append [| address; Array.empty |]
              |> sendAll backend)      
            
            secondary workers.Count cloudCapacity

      let rec primary () =
        let timeout       = if workers.Count = 0 then -1L else 100000L
        let message       = ref None
        let cloudCapacity = ref 0

        let pollset =
          [ Poll( ZMQ.POLLIN,localbe,fun _ ->
                  // handle reply from local worker
                  let msg = recvAll localbe
                  msg.[0] |> workers.Enqueue
                  // if it's READY, don't route the message any further
                  message :=  if msg.[2] = LRU_READY 
                                then None 
                                else msg.[2 ..] |> Some )

            Poll( ZMQ.POLLIN,cloudbe,fun _ ->
                  // handle reply from peer broker
                  let msg = recvAll cloudbe
                  // we don't use peer broker address for anything
                  message := Some(msg.[2 ..]) )

            Poll( ZMQ.POLLIN,statefe,fun _ ->
                  // handle capacity updates
                  cloudCapacity := (recv >> decode >> int) statefe )

            Poll( ZMQ.POLLIN,monitor,fun _ ->
                  // handle monitor message
                  (recv >> decode >> (printfn' "%s")) monitor ) ]
        
        if pollset |> poll timeout then
          !message |> Option.iter (fun msg ->
            let address = decode msg.[0]
            // route reply to cloud if it's addressed to a broker
            // otherwise route reply to client
            msg |> sendAll (if isPeer address then cloudfe else localfe))
        
        //  Now route as many clients requests as we can handle
        let previous = workers.Count
        secondary previous !cloudCapacity

        if workers.Count <> previous then
                  // we stick our own address onto the envelope
          statebe <~| (string >> encode) cloudPort
                  // broadcast new capacity
                  <<| (string >> encode) workers.Count

        primary()
      primary()

      EXIT_SUCCESS
  | _ -> 
      printfn "syntax: peering3 me {you}..."
      EXIT_FAILURE

main fsi.CommandLineArgs
