//
//  Task sink
//  Binds PULL socket to tcp://localhost:5558
//  Collects results from workers via that socket
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Diagnostics;
using ZeroMQ;

namespace zguide.tasksink
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket receiver = context.CreateSocket(SocketType.PULL))
                {
                    receiver.Bind("tcp://*:5558");

                    //  Wait for start of batch
                    receiver.Receive(Encoding.Unicode);

                    var stopwatch = new Stopwatch();
                    stopwatch.Start();

                    const int tasksToConfirm = 100;
                    for (int taskNumber = 0; taskNumber < tasksToConfirm; taskNumber++)
                    {
                        string message = receiver.Receive(Encoding.Unicode);
                        Console.WriteLine(taskNumber % 10 == 0 ? ":" : ".");
                    }

                    stopwatch.Stop();
                    Console.WriteLine("Total elapsed time: {0}", stopwatch.ElapsedMilliseconds);
                }
            }
            Console.ReadKey();
        }
    }
}
