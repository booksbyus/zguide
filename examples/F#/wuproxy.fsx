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

  // hunt messages out to our own subscribers
  while true do
    // process all parts of the message
    let message = frontend |> recvAll
    fflush(); printf "."
    sendAll backend message

  // we don't actually get here but if we did, we'd shut down neatly
  EXIT_SUCCESS

main ()
