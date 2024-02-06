(*
  Pubsub envelope publisher
  Note that the zhelpers.fs file also provides s_sendmore
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"


let main () = 
  // prepare our context and publisher
  use context   = new Context(1)
  use publisher = pub context
  "tcp://*:5563" |> bind publisher

  while true do
    // write two messages, each with an envelope and content
    "A"                         |> s_sendmore  publisher
    "We don't want to see this" |> s_send      publisher
    "B"                         |> s_sendmore  publisher
    "We would like to see this" |> s_send      publisher
    sleep 1

  // we never get here but clean up anyhow
  EXIT_SUCCESS
   
main ()
