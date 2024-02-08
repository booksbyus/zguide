[<AutoOpen>]
module zhelpers

open fszmq
open fszmq.Socket

[<RequireQualifiedAccess>]
module Array = 
  let    last (a:'r array) =  a.[a.Length - 1]
  let tryLast (a:'r array) =  match a.Length with
                              | n when n > 0  ->  Some a.[n - 1]
                              | _             ->  None

exception BadFraming 

let [<Literal>] EXIT_SUCCESS =  0
let [<Literal>] EXIT_FAILURE = -1

let srandom ()  = System.Random(System.DateTime.Now.Millisecond)
let private rnd = srandom() // for use by ``s_setID``

let encode (s:string) = System.Text.Encoding.ASCII.GetBytes(s)
let decode = System.Text.Encoding.ASCII.GetString
let s_send socket s = s |> encode |> send socket
let s_sendmore socket s = s |> encode |> sendMore socket |> ignore
let s_recv socket = socket |> recv |> decode

let printf'  fmt = Printf.kprintf System.Console.Write     fmt
let printfn' fmt = Printf.kprintf System.Console.WriteLine fmt

let scanln = System.Console.ReadLine
let fflush = System.Console.Out.Flush

let sleep (n:int) = System.Threading.Thread.Sleep(n)

let s_clock_start = System.Diagnostics.Stopwatch.StartNew
let s_clock_stop (sw:System.Diagnostics.Stopwatch) = 
  sw.Stop()
  sw.ElapsedMilliseconds

let (|IsChar|IsByte|) = function
    | b when b < 32uy || b > 127uy  -> IsByte(b)
    | c (* is an ASCII character *) -> IsChar(char c)
    
let dumpFrame prefix frame =
  prefix |> Option.fold (fun _ p -> printf' "%s" p) ()
  printf' "[%03d] " (frame |> Array.length)
  let lim = min frame.Length 70
  frame.[ .. (lim - 1)]
  |> Array.iter (function IsChar c -> printf' "%c"   c
                        | IsByte b -> printf' "%02X" b)
  printfn' ""

let dumpMsg msg =
  printfn' "----------------------------------------"
  match msg with
  | null 
  | [||] -> printfn' "<NULL>"
  | msg' -> let lim = min msg'.Length 10
            let dumpFrame' = dumpFrame None
            msg'.[ .. (lim - 1)] |> Array.iter dumpFrame'

let s_dump socket =
  printfn' "----------------------------------------"
  socket |> recvAll |> dumpMsg

let s_setID socket = 
  let identity = sprintf "%04X-%04X" 
                         (rnd.Next(0,0x10000)) 
                         (rnd.Next(0,0x10000))
                 |> encode
  (ZMQ.IDENTITY,identity) |> set socket

let t_spawn fn = 
  let t = System.Threading.Thread(System.Threading.ThreadStart fn)
  t.Start()
  t

let t_spawnp fn p = 
  let fn' = System.Threading.ParameterizedThreadStart fn
  let t = System.Threading.Thread(fn')
  t.Start(p)
  t
