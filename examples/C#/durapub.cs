//
//  Publisher for durable subscriber
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
                using (Socket publisher = context.Socket(SocketType.PUB), sync = context.Socket(SocketType.PULL))
                {
                    publisher.Bind("tcp://*:5565");
                    sync.Bind("tcp://*:5564");

                    //  Wait for synchronization request
                    sync.Recv();

                    for (int updateNumber = 0; updateNumber < 10; updateNumber++)
                    {
                        publisher.Send("Update " + updateNumber, Encoding.Unicode);
                        Thread.Sleep(1000);
                    }

                    publisher.Send("END", Encoding.Unicode);
                }
            }
        }
    }
}
