//
//  Pubsub envelope subscriber
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket subscriber = context.Socket(SocketType.SUB))
                {
                    subscriber.Connect("tcp://localhost:5563");
                    subscriber.Subscribe("B", Encoding.Unicode);

                    while (true)
                    {
                        string address = subscriber.Recv(Encoding.Unicode);
                        string contents = subscriber.Recv(Encoding.Unicode);
                        Console.WriteLine("{0} : {1}", address, contents);
                    }
                }
            }
        }
    }
}
