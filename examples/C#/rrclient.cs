//
//  Request-reply client
//  Connects REQ socket to tcp://localhost:5559
//  Sends "Hello" to server, expects "World" back
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace zguide.rrclient
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket socket = context.CreateSocket(SocketType.REQ))
                {
                    socket.Connect("tcp://localhost:5559");

                    const int requestsToSend = 10;
                    for (int requestNumber = 0; requestNumber < requestsToSend; requestNumber++)
                    {
                        socket.Send("Hello", Encoding.Unicode);
                        string message = socket.Receive(Encoding.Unicode);
                        Console.WriteLine("Received reply: " + message);
                    }
                }
            }
        }
    }
}
