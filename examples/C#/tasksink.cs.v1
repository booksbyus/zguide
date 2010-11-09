using System;
using System.Text;

/**
* Author: Eric Desgranges
* Email: eric@vcardprocessor.com
* License: This example code licensed under the MIT/X11 license.
*/

namespace Sink
{
    class Program
    {
        //
        //  Task sink
        //  Binds PULL socket to tcp://localhost:5558
        //  Collects results from workers via that socket
        //
        static void Main(string[] args)
        {
            using (var context = new ZMQ.Context(1))
            {
                var receiver = context.Socket(ZMQ.PULL);
                receiver.Bind("tcp://*:5558");

                //  Wait for start of batch
                byte[] bytes;
                receiver.Recv(out bytes);

                //  Start our clock now
                DateTime tstart = DateTime.Now;
                Console.WriteLine(string.Format("Started at {0}", tstart.TimeOfDay));

                //  Process 100 confirmations
                for (int task_nbr = 0; task_nbr < 100; task_nbr++)
                {
                    receiver.Recv(out bytes);
                    if ((task_nbr / 10) * 10 == task_nbr)
                        Console.Write(":");
                    else
                        Console.Write(".");
                };
                Console.WriteLine();

                //  Calculate and report duration of batch
                DateTime tend = DateTime.Now;
                Console.WriteLine(string.Format("Ended at {0}", tend.TimeOfDay));
                TimeSpan tdiff = tend - tstart;
                Console.WriteLine();
                Console.WriteLine("Total elapsed time: {0} msec", tdiff.TotalMilliseconds);

                Console.WriteLine("Hit a key to exit");
                Console.ReadKey();
            }
        }
    }
}