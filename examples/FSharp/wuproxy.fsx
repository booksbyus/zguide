(*
Weather proxy device
*)

#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // this is where the weather server sits
  use frontend = context |> sub
  connect frontend "tcp://localhost:5556"

  // this is our public endpoint for subscribers
  use backend = context |> pub
  bind backend "tcp://*:8100"

  // subscribe on everything
  subscribe frontend [""B]

  // shunt messages out to our own subscribers
  while true do
    let more = ref true
    while !more do
      // process all parts of the message
      let message = frontend |> recv
      more := frontend |> recvMore
      if !more then sendMore backend message |> ignore
               else send     backend message
    //NOTE: fs-zmq contains other idioms (eg: sendAll,recvAll,transfer)
    //      which allow for more concise (and possibly more efficient)
    //      implementations of the previous loop... 
    //      but this example translates most directly to it's C cousin
    //      for a very concise alternative, see rrbroker.fsx

  // we don't actually get here but if we did, we'd shut down neatly
  EXIT_SUCCESS

main ()
