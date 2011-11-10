[<AutoOpen>]
module zhelpers

open fszmq
open fszmq.Socket

let [<Literal>] EXIT_SUCCESS = 0

let srandom () = System.Random(System.DateTime.Now.Millisecond)

let encode (s:string) = System.Text.Encoding.ASCII.GetBytes(s)
let decode = System.Text.Encoding.ASCII.GetString
let s_send socket s = s |> encode |> send socket
let s_sendmore socket s = s |> encode |> sendMore socket |> ignore
let s_recv socket = socket |> recv |> decode

let scanln = System.Console.ReadLine
let fflush = System.Console.Out.Flush
let sleep (n:int) = System.Threading.Thread.Sleep(n)

let s_clock_start = System.Diagnostics.Stopwatch.StartNew
let s_clock_stop (sw:System.Diagnostics.Stopwatch) = 
  sw.Stop()
  sw.ElapsedMilliseconds
