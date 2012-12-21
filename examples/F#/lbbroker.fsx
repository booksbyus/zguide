(*
  Least-recently used (LRU) queue device
  Clients and workers are shown here in-process

  While this example runs in a single process, that is just to make
  it easier to start and stop the example. Each thread has its own
  context and conceptually acts as a separate process.
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Polling
open fszmq.Socket

#load "zhelpers.fs"

let [<Literal>] NBR_CLIENTS = 10
let [<Literal>] NBR_WORKERS =  3

open System.Collections.Generic
open System.Threading

// basic request-reply client using REQ socket
// since s_send and s_recv can't handle 0MQ binary identities
// we set a printable text identity to allow routing.
let client_task () =
  use context = new Context(1)
  use client  = req context
  s_setID client // set a printable identity
  "tcp://localhost:5575" |> connect client

  // send request, get reply
  "HELLO"B |>> client
  let reply = s_recv client
  printfn' "Client: %s" reply

// worker using REQ socket to do LRU routing
// since s_send and s_recv can't handle 0MQ binary identities
// we set a printable text identity to allow routing.
let worker_task () =
  use context = new Context(1)
  use worker  = req context
  s_setID worker // set a printable identity
  "tcp://localhost:5585" |> connect worker

  // tell broker we're ready for work
  "READY"B |>> worker

  while true do
    // read and save all frames until we get an empty frame
    // in this example there is only 1 but it could be more
    let address = recv worker
    worker |> recv |> ignore // empty

    // get request, send reply
    let request = s_recv worker
    printfn' "Worker: %s" request

    worker <~| address <~| ""B <<| "OK"B

let main () = 
  // prepare our context and sockets
  use context   = new Context(1)
  use backend   = route context
  use frontend  = route context
  "tcp://*:5585" |> bind backend
  "tcp://*:5575" |> bind frontend

  let client_nbr = ref 0
  while !client_nbr < NBR_CLIENTS do
    let client = Thread(ThreadStart(client_task))
    client.Start()
    incr client_nbr

  for _ in 1 .. NBR_WORKERS do
    let worker = Thread(ThreadStart(worker_task))
    worker.Start()

  (*
    Logic of LRU loop
    - Poll backend always, frontend only if 1+ worker ready
    - If worker replies, queue worker as ready and forward reply
      to client if necessary
    - If client requests, pop next worker and send request to it
  *)

  // queue of available workers
  let worker_queue = Queue<byte[]>()

  // handle worker activity on backend
  let backend_handler _ =
    // queue worker address for LRU routing
    let worker_addr = recv backend
    if worker_queue.Count < NBR_WORKERS then 
      worker_addr |> worker_queue.Enqueue

    // second frame is empty
    backend |> recv |> ignore

    // third frame is READY or else a client address
    let client_addr = recv backend

    // if worker reply, send rest back to frontend
    if client_addr <> "READY"B then
      backend |> recv |> ignore // empty
      let reply = recv backend

      [client_addr; ""B; reply] |> sendAll frontend
      
      decr client_nbr

  // now get next client request, route to LRU worker
  let frontend_handler _ =
    // client request is [address][empty][request]
    let client_addr,request = 
      match frontend |> recvAll with
      | [| address ;_; request |] -> address,request
      | _                         -> failwith "invalid client request"
    
    let worker_addr = worker_queue.Dequeue()  
    [ worker_addr; ""B; client_addr; ""B; request ] |> sendAll backend

  let backend_poll,frontend_poll =
    Poll(ZMQ.POLLIN,backend ,backend_handler ),
    Poll(ZMQ.POLLIN,frontend,frontend_handler)

  while !client_nbr > 0 do
    [ yield backend_poll
      if worker_queue.Count > 0 then yield frontend_poll ]
    |> poll -1L
    |> ignore

  EXIT_SUCCESS
   
main ()
