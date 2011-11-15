[<AutoOpen>]
module zhelpers

open fszmq
open fszmq.Socket

let [<Literal>] EXIT_SUCCESS = 0

let srandom ()  = System.Random(System.DateTime.Now.Millisecond)
let private rnd = srandom() // for use by ``s_setID``

let encode (s:string) = System.Text.Encoding.ASCII.GetBytes(s)
let decode = System.Text.Encoding.ASCII.GetString
let s_send socket s = s |> encode |> send socket
let s_sendmore socket s = s |> encode |> sendMore socket |> ignore
let s_recv socket = socket |> recv |> decode

let print' (s:string) = System.Console.WriteLine("{0}",s)
let scanln = System.Console.ReadLine
let fflush = System.Console.Out.Flush

let sleep (n:int) = System.Threading.Thread.Sleep(n)

let s_clock_start = System.Diagnostics.Stopwatch.StartNew
let s_clock_stop (sw:System.Diagnostics.Stopwatch) = 
  sw.Stop()
  sw.ElapsedMilliseconds

let s_dump socket =
  
  let (|IsChar|IsByte|) = function
    | b when b < 32uy || b > 127uy  -> IsByte(b)
    | c (* is an ASCII character *) -> IsChar(char c)
    
  let dumpFrame frame =
    printf "[%03d] " (frame |> Array.length)
    frame 
    |> Array.iter (function IsChar c -> printf "%c"   c
                          | IsByte b -> printf "%02X" b)
    print' ""

  print' "----------------------------------------"
  socket |> recvAll |> Array.iter dumpFrame

let s_setID socket = 
  let identity = sprintf "%04X-%04X" 
                         (rnd.Next(0,0x10000)) 
                         (rnd.Next(0,0x10000))
                 |> encode
  (ZMQ.IDENTITY,identity) |> set socket
