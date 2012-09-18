
//
//  Clone server, Model One in C#
//

//  Author:     Yan Cui
//  Email:      theburningmonk@gmail.com

using System;
using System.Collections.Generic;
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
                using (var publisher = context.Socket(SocketType.PUB))
                {
                    publisher.Bind("tcp://*:5556");
                    Thread.Sleep(TimeSpan.FromMilliseconds(200));

                    var interrupted = false;
                    var sequence = 0L;
                    var random = new Random((int)DateTime.UtcNow.Ticks);
                    var dict = new Dictionary<string, KvMsg>();

                    Console.CancelKeyPress += (s, e) => { interrupted = true; };

                    while (!interrupted)
                    {
                        sequence++;
                        var kvmsg = new KvMsg(sequence);
                        kvmsg.Key = random.Next(1, 10000).ToString();
                        kvmsg.Body = random.Next(1, 1000000).ToString();

                        kvmsg.Send(publisher);
                        kvmsg.Store(dict);

                        Console.WriteLine("Published {0}", kvmsg);

                        Thread.Sleep(TimeSpan.FromMilliseconds(100));
                    }

                    Console.WriteLine(" Interrupted\n{0} messages out\n", sequence);
                    Console.ReadKey();
                }
            }
        }
    }
}
