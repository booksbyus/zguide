(*
  Synchronized subscriber
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

   // first, connect our subscriber socket
  use subscriber = sub context
  "tcp://localhost:5561" |> connect subscriber
  [ ""B ] |> subscribe subscriber

  // 0MQ is so fast, we need to wait a while...
  sleep 1

  // second, synchronize with publisher
  use syncclient = req context
  "tcp://localhost:5562" |> connect syncclient

  // - send a synchronization request
  "" |> s_send syncclient

  // - wait for synchronization reply
  syncclient |> s_recv |> ignore

  // third, get our updates and report how many we got
  let rec loop count =
    let message = s_recv subscriber
    if  message <> "END" 
      then loop (count + 1)
      else count
  let update_nbr = loop 0
  printfn "Received %d updates" update_nbr

  EXIT_SUCCESS
   
main ()
