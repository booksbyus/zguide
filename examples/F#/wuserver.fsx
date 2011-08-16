(*
Weather update server
Binds PUB socket to tcp://*:5556
Publishes random weather updates
*)

#r "bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  // prepare our context and publisher
  use context   = new Context(1);
  use publisher = context |> Context.pub
  "tcp://*:5556" |> Socket.bind publisher
  // "icp://weather.ipc" |> Socket.bind publisher
  //NOTE: IPC transport is not currently supported on Microsoft Windows

  // initialize random number generator
  let rand = srandom ()
  while true do
    // get values that will fool the boss
    let zipcode, temperature, relhumidity =
      rand.Next 100000, (rand.Next 215) - 80, (rand.Next 50) + 10
    
    // send message to all subscribers
    let update = sprintf "%05d %d %d" zipcode temperature relhumidity
    update |> s_send publisher

  EXIT_SUCCESS

main ()
