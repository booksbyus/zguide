//
//  Hello World client
//  Connects REQ socket to tcp://localhost:5555
//  Sends "Hello" to server, expects "World" back
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace zguide.hwclient
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket requester = context.CreateSocket(SocketType.REQ))
                {
                    requester.Connect("tcp://localhost:5555");

                    const string requestMessage = "Hello";
                    const int requestsToSend = 10;

                    for (int requestNumber = 0; requestNumber < requestsToSend; requestNumber++)
                    {
                        Console.WriteLine("Sending request {0}...", requestNumber);
                        requester.Send(requestMessage, Encoding.Unicode);

                        string reply = requester.Receive(Encoding.Unicode);
                        Console.WriteLine("Received reply {0}: {1}", requestNumber, reply);
                    }
                }
            }
        }
    }
}
