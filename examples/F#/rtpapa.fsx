(*
  Custom routing Router to Papa (ROUTER to REP)
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

// we will do this all in one thread to emphasize the sequence of events...
let main () = 
  use context = new Context(1)

  use client = route context
  "tcp://*:5572" |> bind client

  use worker = rep context
  (ZMQ.IDENTITY,"A"B) |> set worker
  "tcp://localhost:5572" |> connect worker

  // wait for the worker to connect so that when we send a message
  // with routing envelope, it will actually match the worker...
  sleep 100

  // send papa address, address stack, empty part, and request
  [ "A"B;
    "address 3"B;
    "address 2"B;
    "address 1"B;
    ""B;
    "This is the workload"B ] |> sendAll client

  // worker should get just the workload
  s_dump worker

  // we don't play with envelopes in the worker
  "This is the reply"B |>> worker

  // now dump what we got off the ROUTER socke...
  s_dump client

  EXIT_SUCCESS
   
main ()
