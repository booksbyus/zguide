(*
  Demonstrate identities as used by the request-reply pattern.  Run this
  program by itself. Note that the utility functions s_ are provided by
  zhelpers.fs. It gets boring for everyone to keep repeating this code.
*)
#r @"bin/fszmq.dll"
open fszmq
open fszmq.Context
open fszmq.Socket

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  use sink = route context
  "inproc://example" |> bind sink

  // first allow 0MQ to set the identity
  use anonymous = req context
  "inproc://example" |> connect anonymous
  "ROUTER uses a generated UUID" |> s_send anonymous
  s_dump sink

  // then set the identity ourself
  use identified = req context
  (ZMQ.IDENTITY,"Hello"B) |> set identified
  "inproc://example" |> connect identified
  "ROUTER socket uses REQ's socket identity" |> s_send identified
  s_dump sink

  EXIT_SUCCESS
   
main ()
