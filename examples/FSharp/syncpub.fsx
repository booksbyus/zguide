(*
  Synchronized publisher
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

// we wait for 10 subscribers
let [<Literal>] SUBSCRIBERS_EXPECTED = 10

let main () = 
  use context = new Context(1)

  // socket to talk to clients
  use publisher = pub context
  "tcp://*:5561" |> bind publisher

  // socket to receive signals
  use syncservice = rep context
  "tcp://*:5562" |> bind syncservice

  // get synchronization from subscribers
  printfn "Waiting for subscribers"
  let subscribers = ref 0
  while !subscribers < SUBSCRIBERS_EXPECTED do
    // - wait for synchronization request
    syncservice |> s_recv |> ignore
    // - send synchronization reply
    "" |> s_send syncservice
    incr subscribers

  // now broadcast exactly 1M updates followed by END
  printfn "Broadcasting messages"
  for update_nbr in 0 .. 999999 do "Rhubarb" |> s_send publisher
  
  "END" |> s_send publisher

  EXIT_SUCCESS
   
main ()
