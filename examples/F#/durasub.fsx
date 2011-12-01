(*
  Durable subscriber
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // connect our subscriber socket
  use subscriber = sub context
  (ZMQ.IDENTITY,"Hello"B) |> set        subscriber
  [ ""B ]                 |> subscribe  subscriber
  "tcp://localhost:5565"  |> connect    subscriber

  // synchronize with publisher
  use sync = push context
  "tcp://localhost:5564" |> connect sync
  "" |> s_send sync

  // get updates, exit when told to do so
  let rec loop () =
    let message = s_recv subscriber
    printfn "%s" message
    if message <> "END" then loop()
  loop()

  EXIT_SUCCESS
   
main ()
