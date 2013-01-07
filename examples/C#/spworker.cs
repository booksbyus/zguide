//
//  Simple Pirate worker
//  Connects REQ socket to tcp://*:5556
//  Implements worker part of LRU queueing
//
//  Author: Kristian Kristensen <kristian@kristenseninc.com>
//

using System;
using System.Text;
using ZeroMQ;
using zguide;

namespace zguide.spworker
{
    class Program
    {
        private const string LRU_READY = "READY";

        static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (var worker = context.CreateSocket(SocketType.REQ))
                {
                    var randomizer = new Random(DateTime.Now.Millisecond);
                    var identity = ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5556");

                    Console.WriteLine("I: {0} worker ready", identity);
                    worker.Send("READY", Encoding.Unicode);

                    var cycles = 0;
                    while (true)
                    {
                        var zmsg = new ZMessage(worker);

                        cycles += 1;
                        if (cycles > 3 && randomizer.Next(0, 5) == 0)
                        {
                            Console.WriteLine("I: {0} simulating a crash", identity);
                            break;
                        }
                        else if (cycles > 3 && randomizer.Next(0, 5) == 0)
                        {
                            Console.WriteLine("I: {0} simulating CPU overload", identity);
                            System.Threading.Thread.Sleep(3000);
                        }
                        Console.WriteLine("I: {0} normal reply", identity);
                        System.Threading.Thread.Sleep(1000);
                        zmsg.Send(worker);
                    }
                }
            }
        }
    }
}