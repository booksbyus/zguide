//
//  Broker peering simulation (part 1) in C#
//  Prototypes the state flow
//  Note! ipc doesnt work on windows and therefore type peering1 8001 8002 8003

//  Author:     Tomas Roos
//  Email:      ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.peering1
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            var randomizer = new Random(DateTime.Now.Millisecond);

            if (args.Length < 2)
            {
                Console.WriteLine("Usage: peering1 <myself> <peer_1> ... <peer_N>");
                return;
            }

            var myself = args[0];
            Console.WriteLine("Hello, I am " + myself);

            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket statebe = context.CreateSocket(SocketType.PUB), statefe = context.CreateSocket(SocketType.SUB))
                {
                    var bindAddress = "tcp://127.0.0.1:" + myself;
                    statebe.Bind(bindAddress);
                    Thread.Sleep(1000);

                    for (int arg = 1; arg < args.Length; arg++)
                    {
                        var endpoint = "tcp://127.0.0.1:" + args[arg];
                        statefe.Connect(endpoint);
                        statefe.Subscribe(Encoding.Unicode.GetBytes(string.Empty));
                        Thread.Sleep(1000);
                    }

                    statefe.ReceiveReady += (s, e) =>
                                                 {
                                                     string peerName = e.Socket.Receive(Encoding.Unicode);
                                                     string available = e.Socket.Receive(Encoding.Unicode);

                                                     Console.WriteLine("{0} - {1} workers free\n", peerName, available);
                                                 };

                    var poller = new Poller(new List<ZmqSocket> { statefe });

                    while (true)
                    {
                        int count = poller.Poll(TimeSpan.FromMilliseconds(1000 * 1000));
                        
                        if (count == 0)
                        {
                            statebe.Send(myself, Encoding.Unicode);
                            statebe.Send(randomizer.Next(10).ToString(), Encoding.Unicode);
                        }
                    }
                }
            }
        }
    }
}
