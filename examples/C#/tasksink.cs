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
using ZMQ;

namespace ZMQGuide 
{
    internal class Program 
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket receiver = context.Socket(SocketType.PULL))
                {
                    receiver.Bind("tcp://*:5558");

                    //  Wait for start of batch
                    receiver.Recv();

                    var stopwatch = new Stopwatch();
                    stopwatch.Start();

                    const int tasksToConfirm = 100;
                    for (int taskNumber = 0; taskNumber < tasksToConfirm; taskNumber++)
                    {
                        string message = receiver.Recv(Encoding.Unicode);
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
