//
//  Request-reply service
//  Connects REP socket to tcp://localhost:5560
//  Expects "Hello" from client, replies with "World"
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace zguide.rrworker
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket socket = context.CreateSocket(SocketType.REP))
                {
                    socket.Connect("tcp://localhost:5560");

                    while (true)
                    {
                        string message = socket.Receive(Encoding.Unicode);
                        Console.WriteLine("Received request: " + message);
                        socket.Send("World", Encoding.Unicode);
                    }
                }
            }
        }
    }
}
