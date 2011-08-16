(*
Hello World client
Connects REQ socket to tcp://localhost:5555
Sends "Hello" to server, expects "World" back
*)

#r @"bin/fszmq.dll"
open fszmq

let main () = 
  use context = new Context(1)

  // socket to talk to server
  printfn "Connecting to hello world server..."
  use requester = context |> Context.req
  "tcp://localhost:5555" |> Socket.connect requester

  for request_nbr in 0 .. 99 do
    printfn "Sending Hello %d..." request_nbr
    "Hello"B |> Socket.send requester

    requester |> Socket.recv |> ignore
    printfn "Received World %d" request_nbr
    
  0 (* RETURN CODE *)
   
main ()
