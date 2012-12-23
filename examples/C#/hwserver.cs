//
//  Hello World server
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.hwserver
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket replyer = context.CreateSocket(SocketType.REP))
                {
                    replyer.Bind("tcp://*:5555");

                    const string replyMessage = "World";

                    while (true)
                    {
                        string message = replyer.Receive(Encoding.Unicode);
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
