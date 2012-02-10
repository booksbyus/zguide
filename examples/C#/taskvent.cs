//
//  Task ventilator
//  Binds PUSH socket to tcp://localhost:5557
//  Sends batch of tasks to workers via that socket
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
                using (Socket sender = context.Socket(SocketType.PUSH))
                {
                    sender.Bind("tcp://*:5557");

                    Console.WriteLine("Press enter when the workers are ready: ");

                    while (Console.ReadKey(true).Key != ConsoleKey.Enter)
                    {

                    }

                    Console.WriteLine("Sending tasks to workers...");

                    //  The first message is "0" and signals start of batch
                    sender.Send("0", Encoding.Unicode);

                    var randomizer = new Random(DateTime.Now.Millisecond);

                    const int tasksToSend = 100;

                    int expectedTime = 0;

                    for (int taskNumber = 0; taskNumber < tasksToSend; taskNumber++)
                    {
                        //  Random workload from 1 to 100msecs
                        int sleepTimeOnWorker = randomizer.Next(1, 100);
                        expectedTime += sleepTimeOnWorker;
                        sender.Send(sleepTimeOnWorker.ToString(), Encoding.Unicode);
                    }

                    Console.WriteLine("Total expected time for 1 worker: {0} msec", expectedTime);
                }
            }

            Console.ReadKey();
        }
    }
}
