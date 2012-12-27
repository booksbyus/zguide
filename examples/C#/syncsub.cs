//
//  Synchronized subscriber
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.syncsub
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket subscriber = context.CreateSocket(SocketType.SUB), syncClient = context.CreateSocket(SocketType.REQ))
                {
                    subscriber.Connect("tcp://localhost:5561");
                    subscriber.Subscribe(Encoding.Unicode.GetBytes(string.Empty));

                    syncClient.Connect("tcp://localhost:5562");

                    //  - send a synchronization request
                    syncClient.Send("", Encoding.Unicode);
                    //  - wait for synchronization reply
                    syncClient.Receive(Encoding.Unicode);

                    int receivedUpdates = 0;
                    while (!subscriber.Receive(Encoding.Unicode).Equals("END"))
                    {
                        receivedUpdates++;
                    }

                    Console.WriteLine("Received {0} updates.", receivedUpdates);
                }
            }

            Console.ReadKey();
        }
    }
}
