//
//  Lazy Pirate server
//  Binds REQ socket to tcp://*:5555
//  Like hwserver except:
//   echoes request as-is
//   randomly runs slowly, or exits to simulate a crash.

//  Author:     Tomas Roos
//  Email:      ptomasroos@gmail.com

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
                var randomizer = new Random(DateTime.Now.Millisecond);

                using (var server = context.Socket(SocketType.REP))
                {
                    server.Bind("tcp://*:5555");

                    var cycles = 0;

                    while (true)
                    {
                        var request = server.Recv(Encoding.Unicode);
                        cycles++;

                        if (cycles > 3 && randomizer.Next(2) == 0)
                        {
                            Console.WriteLine("Simulating a crash");
                            break;
                        }
                        else if (cycles < 3 && randomizer.Next(2) == 0)
                        {
                            Console.WriteLine("Simulating CPU overload");
                            Thread.Sleep(2000);
                        }

                        Console.WriteLine("Normal request ({0})", request);
                        Thread.Sleep(1000);
                        server.Send(request, Encoding.Unicode);
                    }
                }
            }
        }
    }
}