(*
Weather update client
Connects SUB socket to tcp://localhost:5556
Collects weather updates and finds avg temp in zipcode
*)

#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  use context = new Context(1)

  // socket to talk to server
  printfn "Collecting updates from weather server..."
  use subscriber = context |> Context.sub
  Socket.connect subscriber "tcp://localhost:5556"

  // subscribe to zipcode, default is NYC, 10001
  let filter =  if fsi.CommandLineArgs.Length = 2 
                  then fsi.CommandLineArgs.[1] 
                  else "10001"
  printfn "Listening for updates from: '%s'" filter
  Socket.subscribe subscriber [ encode filter ]

  // process 100 updates
  let update_nbr = ref 0
  let total_temp = ref 0
  while !update_nbr < 100 do
    let update = s_recv subscriber
    let zipcode, temperature, relhumidity =
      let update' = update.Split()
      (int update'.[0]),(int update'.[1]),(int update'.[2])
    total_temp := !total_temp + temperature
    incr update_nbr
  
  printfn "\nAverage temperature for zipcode '%s' was %dF"
          filter 
          (!total_temp / !update_nbr)

  EXIT_SUCCESS
   
main ()
