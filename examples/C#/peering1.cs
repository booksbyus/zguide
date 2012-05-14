//
//  Broker peering simulation (part 1) in C#
//  Prototypes the state flow
//  Note! ipc doesnt work on windows and therefore type peering1 8001 8002 8003

//  Author:     Tomas Roos
//  Email:      ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            var randomizer = new Random(DateTime.Now.Millisecond);

            if (args.Length < 2)
            {
                Console.WriteLine("Usage: peering1 <myself> <peer_1> … <peer_N>");
                return;
            }

            var myself = args[0];
            Console.WriteLine("Hello, I am " + myself);

            using (var context = new Context(1))
            {
                using (Socket statebe = context.Socket(SocketType.PUB), statefe = context.Socket(SocketType.SUB))
                {
                    var bindAddress = "tcp://127.0.0.1:" + myself;
                    statebe.Bind(bindAddress);
                    Thread.Sleep(1000);

                    for (int arg = 1; arg < args.Length; arg++)
                    {
                        var endpoint = "tcp://127.0.0.1:" + args[arg];
                        statefe.Connect(endpoint);
                        statefe.Subscribe(string.Empty, Encoding.Unicode);
                        Thread.Sleep(1000);
                    }

                    statefe.PollInHandler += (socket, revents) =>
                                                 {
                                                     string peerName = socket.Recv(Encoding.Unicode);
                                                     string available = socket.Recv(Encoding.Unicode);

                                                     Console.WriteLine("{0} - {1} workers free\n", peerName, available);
                                                 };

                    while (true)
                    {
                        int count = Context.Poller(1000 * 1000, statefe);
                        
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
