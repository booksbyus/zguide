//
//  Pubsub envelope subscriber
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program7
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket subscriber = context.CreateSocket(SocketType.SUB))
                {
                    subscriber.Connect("tcp://localhost:5563");
                    subscriber.Subscribe("B", Encoding.Unicode);

                    while (true)
                    {
                        string address = subscriber.Receive(Encoding.Unicode);
                        string contents = subscriber.Receive(Encoding.Unicode);
                        Console.WriteLine("{0} : {1}", address, contents);
                    }
                }
            }
        }
    }
}
