(*
  Pubsub envelope subscriber
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"
let main () = 
   // prepare our context and publisher
  use context    = new Context(1)
  use subscriber = sub context
  "tcp://localhost:5563" |> connect subscriber
  [ "B"B ] |> subscribe subscriber

  while true do
    // read envelope with address
    let address  = s_recv subscriber
    // read message contents
    let contents = s_recv subscriber
    printfn "[%s] %s" address contents

  // we never get here but clean up anyhow
  EXIT_SUCCESS

main ()
