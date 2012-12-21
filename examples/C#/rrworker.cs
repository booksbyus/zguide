//
//  Request-reply service
//  Connects REP ZmqSocket to tcp://localhost:5560
//  Expects "Hello" from client, replies with "World"
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program17
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket ZmqSocket = context.CreateSocket(SocketType.REP))
                {
                    ZmqSocket.Connect("tcp://localhost:5560");

                    while (true)
                    {
                        string message = ZmqSocket.Receive(Encoding.Unicode);
                        Console.WriteLine("Received request: " + message);
                        ZmqSocket.Send("World", Encoding.Unicode);
                    }
                }
            }
        }
    }
}
