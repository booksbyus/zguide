(*
Hello World server
Binds REP socket to tcp://*:5555
Expects "Hello" from client, replies with "World"
*)

#r @"bin/fszmq.dll"
open fszmq

let main () = 
  use context = new Context(1)
  
  // socket to talk to clients
  use responder = context |> Context.rep
  "tcp://*:5555" |> Socket.bind responder

  while true do
    // wait for next request from client
    responder |> Socket.recv |> ignore
    printfn "Received Hello"

    // do some work
    System.Threading.Thread.Sleep 1

    // send reply back to client
    "World"B |> Socket.send responder
  
  // we never get here but if we did, this would be how we end
  0 (* return code *)

main ()
