(*
  Broker peering simulation (part 1)
  Prototypes the state flow
*)
#r @"bin/fszmq.dll"
#r @"bin/fszmq.devices.dll"
open fszmq
open fszmq.Context
open fszmq.devices
open fszmq.Polling
open fszmq.Socket

#load "zhelpers.fs"

let main args =
  // first argument is this broker's name
  // other arguments are our peers' names
  match args |> Array.length with
  | n when n > 1 ->
      let rand = srandom()
      let self = args.[1]
      printfn "I: preparing broker at %s..." self

      // prepare our context and sockets
      use ctx = new Context(1)
      use statebe = ctx |> pub
      bind statebe (sprintf "tcp://*:%s" self)
      //NOTE: to run this example on Windows, we must use tcp... 
      //      so when we do, assume inputs are port numbers
      //      on non-windows systems, we can use ipc (as per the guide)...
      //      so in *that* case, inputs are alphanumeric identifiers, eg:
      //
      //          bind statebe (sprintf "ipc://%s-state.ipc" self)
      //

      // connect statefe to all peers
      use statefe = ctx |> sub
      [""B] |> subscribe statefe
      args.[2..] |> Array.iter (fun peer -> 
          printfn "I: connecting to state backend at '%s'" peer
          connect statefe (sprintf "tcp://localhost:%s" peer))
          //NOTE: see previous note about Windows and ipc vs. tcp

      // send out status messages to peers, and collect from peers
      // the zmq_poll timeout defines our own heartbeating
      let items =
        [ Poll(ZMQ.POLLIN,statefe,fun _ ->
                let peer_name = statefe |> recv |> decode
                let available = statefe |> recv |> decode
                printfn "%s - %s workers free" peer_name available) ]
      while true do
        if not (items |> poll 10000L) then
          // send random value for worker availability
          statebe <~| (encode self)
                  <<| (rand.Next(0,10) |> string |> encode)

      EXIT_SUCCESS
  | _ -> 
      printfn "syntax: peering1 me {you}..."
      EXIT_FAILURE

main fsi.CommandLineArgs
