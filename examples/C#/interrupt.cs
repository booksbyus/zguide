//
//  Shows how to handle Ctrl-C
//

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
            using (var context = new Context(1))
            {
                using (Socket replyer = context.Socket(SocketType.REP))
                {
                    replyer.Bind("tcp://*:5555");

                    bool interrupted = false;

                    Console.CancelKeyPress += delegate { interrupted = true; };

                    const string replyMessage = "World";

                    while (!interrupted)
                    {
                        string message = replyer.Recv(Encoding.Unicode);
                        Console.WriteLine("Received request: {0}", message);

                        // Simulate work, by sleeping
                        Thread.Sleep(1000);

                        // Send reply back to client
                        replyer.Send(replyMessage, Encoding.Unicode);
                    }
                }
            }
        }
    }
}