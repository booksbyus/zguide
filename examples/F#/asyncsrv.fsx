(*
  Asynchronous client-to-server (DEALER to ROUTER)

  While this example runs in a single process, that is just to make
  it easier to start and stop the example. Each task has its own
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

// this is our client task
// it connects to the server, and then sends a request once per second
// it collects responses as they arrive, and it prints them out. We will
// run several client tasks in parallel, each with a different random ID.
let client_task () =
  use ctx     = new Context(1)
  use client  = deal ctx

  // set random identity to make tracing easier
  s_setID client
  let identity = ZMQ.IDENTITY |> get<byte[]> client |> decode
  connect client "tcp://localhost:5570"
  
  let printMsg socket = 
    let content = match socket |> recvAll with
                  | [| content |] -> decode content
                  | _             -> "<NULL>"
    printfn' "(%s) %s" identity content

  let request_nbr = ref 0
  while true do
    // tick once per second, pulling in arriving messages
    for _ in 1 .. 100 do
      [Poll(ZMQ.POLLIN,client,printMsg)] |> poll 10000L |> ignore
    incr request_nbr
    (sprintf "request %d" !request_nbr) |> s_send client

// accept a request and reply with the same text 
// a random number of times, with random delays between replies.
let rand = srandom()
let server_worker (ctx:obj) = 
  use worker = (ctx :?> Context) |> deal
  connect worker "tcp://localhost:5600"

  while true do
    // The DEALER socket gives us the address envelope and message
    let message = worker |> recvAll
    //  Send 0..4 replies back
    let replies = rand.Next(0,5)
    for _ in 1 .. replies do
      sleep (rand.Next 1000)
      message |> sendAll worker

// this is our server task
// it uses the multithreaded server model to deal requests out to a pool
// of workers and route replies back to clients. One worker can handle
// one request at a time but one client can talk to multiple workers at
// once.
let server_task () =
  use ctx = new Context(1)

  // frontend socket talks to clients over TCP
  use frontend = ctx |> route
  bind frontend "tcp://*:5570"

  // backend socket talks to workers over inproc
  use backend = ctx |> deal
  // bind backend "inproc://backend"
  // ... except on Windows where 0MQ doesn't have a binding 
  //     for named pipes, so we use TCP instead
  bind backend "tcp://*:5600"

  // launch pool of worker threads, precise number is not critical
  for _ in 1 .. 5 do 
    ctx |> s_spawnp server_worker |> ignore
  
  // connect backend to frontend via a queue device
  // we could do this:
  //     Devices.queue(frontend,backend)
  // but doing it ourselves means we can debug this more easily

  // switch messages between frontend and backend
  let items = 
    [ Poll(ZMQ.POLLIN,frontend,
           fun _ -> let msg = frontend |> recvAll
                    //printfn' "request from client:"
                    //dumpMsg msg
                    msg |> sendAll backend)
      Poll(ZMQ.POLLIN,backend ,
           fun _ -> let msg = backend |> recvAll
                    //printfn' "reply from worker:"
                    //dumpMsg msg
                    msg |> sendAll frontend) ]
  while items |> poll -1L do ((* loop *))

let main () =
  s_spawn client_task |> ignore
  s_spawn client_task |> ignore
  s_spawn client_task |> ignore
  s_spawn server_task |> ignore
  // run for 5 seconds then quit
  sleep 5000
  EXIT_SUCCESS
   
main ()
