//
//  Synchronized subscriber
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

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
                using (Socket subscriber = context.Socket(SocketType.SUB), syncClient = context.Socket(SocketType.REQ))
                {
                    subscriber.Connect("tcp://localhost:5561");
                    subscriber.Subscribe("", Encoding.Unicode);

                    syncClient.Connect("tcp://localhost:5562");

                    //  - send a synchronization request
                    syncClient.Send("", Encoding.Unicode);
                    //  - wait for synchronization reply
                    syncClient.Recv();

                    int receivedUpdates = 0;
                    while (!subscriber.Recv(Encoding.Unicode).Equals("END"))
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
