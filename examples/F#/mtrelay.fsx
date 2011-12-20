(*
  Multithreaded relay
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

open System.Threading

let step1 (o:obj) =
  // connect to step2 and tell it we're ready
  use xmitter = (o :?> Context) |> pair
  "inproc://step2" |> connect xmitter
  printfn "Step 1 ready, signaling step 2"
  "READY" |> s_send xmitter

let step2 (o:obj) =
  let context : Context = downcast o
  // bind inproc socket before starting step1
  use receiver = pair context
  "inproc://step2" |> bind receiver
  let t = Thread(ParameterizedThreadStart step1)
  t.Start(o)

  // wait for signal and pass it on
  s_recv receiver |> ignore

  // connect to step3 and tell it we're ready
  use xmitter = pair context
  "inproc://step3" |> connect xmitter
  printfn "Step 2 ready, signaling step 3"
  "READY" |> s_send xmitter

let main () = 
  use context = new Context(1)

  // bind inproc socket before starting step2
  use receiver = pair context
  "inproc://step3" |> bind receiver
  let t = Thread(ParameterizedThreadStart step2)
  t.Start(context)

  // wait for signal
  s_recv receiver |> ignore
  
  printfn "Test successful"
  EXIT_SUCCESS
   
main ()
