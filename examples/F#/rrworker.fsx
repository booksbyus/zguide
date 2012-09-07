(*
  Hello World server
  Connects REP socket to tcp://*:5560
  Expects "Hello" from client, replies with "World"
*)

#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // socket to talk to clients
  use responder = rep context
  "tcp://localhost:5560" |> connect responder

  while true do
    // wait for next request from client
    let message = s_recv responder
    printfn "Received request: [%s]" message
    
    // do some 'work'
    sleep 1

    // send reply back to client
    "World" |> s_send responder

  // we never get here but clean up anyhow
  EXIT_SUCCESS

main ()
