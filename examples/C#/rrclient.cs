//
//  Request-reply client
//  Connects REQ ZmqSocket to tcp://localhost:5559
//  Sends "Hello" to server, expects "World" back
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program18
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket ZmqSocket = context.CreateSocket(SocketType.REQ))
                {
                    ZmqSocket.Connect("tcp://localhost:5559");

                    const int requestsToSend = 10;
                    for (int requestNumber = 0; requestNumber < requestsToSend; requestNumber++)
                    {
                        ZmqSocket.Send("Hello", Encoding.Unicode);
                        string message = ZmqSocket.Receive(Encoding.Unicode);
                        Console.WriteLine("Received reply: " + message);
                    }
                }
            }
        }
    }
}
