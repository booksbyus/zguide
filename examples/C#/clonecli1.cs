
//
//  Clone client, Model One in C#
//

//  Author:     Yan Cui
//  Email:      theburningmonk@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using ZeroMQ;

namespace zguide.clonecli1
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (var subscriber = context.CreateSocket(SocketType.SUB))
                {
                    subscriber.Connect("tcp://localhost:5556");
                    subscriber.Subscribe(Encoding.Unicode.GetBytes(string.Empty));

                    var interrupted = false;
                    var sequence = 0L;
                    var dict = new Dictionary<string, KvMsg>();

                    Console.CancelKeyPress += (s, e) => { interrupted = true; };

                    while (!interrupted)
                    {
                        KvMsg kvmsg;

                        try
                        {
                            kvmsg = KvMsg.Receive(subscriber);
                            Console.WriteLine("Received {0}", kvmsg);
                        }
                        catch (System.Exception)
                        {
                            break;
                        }

                        kvmsg.Store(dict);
                        sequence++;
                    }

                    Console.WriteLine(" Interrupted\n{0} messages in\n", sequence);
                    Console.ReadKey();
                }
            }
        }
    }
}
