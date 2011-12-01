(*
  Hello World client
  Connects REQ socket to tcp://localhost:5559
  Sends "Hello" to server, expects "World" back
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // socket to talk to server
  use requester = req context
  "tcp://localhost:5559" |> connect requester

  for request_nbr in 0 .. 9 do
    "Hello" |> s_send requester
    let message = s_recv requester
    printfn "Received reply %d [%s]" request_nbr message

  EXIT_SUCCESS

main ()
