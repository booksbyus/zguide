(*
Report 0MQ version
*)

#r @"bin/fszmq.dll"
open fszmq

#load "zhelpers.fs"

let main () = 
  match ZMQ.version with
  | Version(m,n,p)  -> printfn "Current 0MQ version is %d.%d.%d" m n p
  | Unknown         -> printfn "Unable to determine current 0MQ version"
  EXIT_SUCCESS
   
main ()
