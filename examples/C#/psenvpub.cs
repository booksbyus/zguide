﻿//
//  Pubsub envelope publisher
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.psenvpub
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket publisher = context.CreateSocket(SocketType.PUB))
                {
                    publisher.Bind("tcp://*:5563");

                    while (true)
                    {
                        //  Write two messages, each with an envelope and content
                        publisher.SendMore("A", Encoding.Unicode);
                        publisher.Send("We don't want to see this.", Encoding.Unicode);
                        publisher.SendMore("B", Encoding.Unicode);
                        publisher.Send("We would like to see this.", Encoding.Unicode);
                        Thread.Sleep(1000); // avoid flooding the publisher
                    }
                }
            }
        }
    }
}
