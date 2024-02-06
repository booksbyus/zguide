(*
  Simple request-reply broker
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Polling
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  // prepare our context and sockets
  use context   = new Context(1)
  use frontend  = route context
  use backend   = deal context
  "tcp://*:5559" |> bind frontend
  "tcp://*:5560" |> bind backend
 
  // initialize poll set
  let items = [Poll(ZMQ.POLLIN,frontend,fun s -> s >|< backend )
               Poll(ZMQ.POLLIN,backend ,fun s -> s >|< frontend)]
  //NOTE: the poll item callbacks above use the transfer operator (>|<).
  //      fs-zmq defines this operator as a convenience for transferring
  //      all parts of a multi-part message from one socket to another. 
  //      for a lengthier, but more obvious alternative (which more 
  //      closely matches the C version of the guide), see wuproxy.fsx

  // switch messages between sockets
  while true do items |> poll -1L |> ignore
  
  // we never get here but clean up anyhow
  EXIT_SUCCESS

main ()
