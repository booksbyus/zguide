using System;
using System.Text;

/**
* Author: Eric Desgranges
* Email: eric@vcardprocessor.com
* License: This example code licensed under the MIT/X11 license.
*/

namespace Server
{
    class Program
    {
        //
        //  Task ventilator
        //  Binds PUSH socket to tcp://localhost:5557
        //  Sends batch of tasks to workers via that socket
        //
        static void Main()
        {
            using (var context = new ZMQ.Context(1))
            {
                //  Socket to send messages on
                var sender = context.Socket(ZMQ.PUSH);
                sender.Bind("tcp://*:5557");

                Console.WriteLine("Press Enter when the workers are ready: ");
                Console.ReadKey();
                Console.WriteLine("Sending tasks to workers...");

                //  The first message is "0" and signals start of batch
                sender.Send(Encoding.ASCII.GetBytes("0"));

                //  Initialize random number generator
                Random random = new Random();

                //  Send 100 tasks
                int total_msec = 0;     //  Total expected cost in msecs
                for (int task_nbr = 0; task_nbr < 100; task_nbr++) {
                    int workload;
                    //  Random workload from 1 to 100msecs
                    workload = random.Next(100) + 1;
                    total_msec += workload;
                    sender.Send(Encoding.ASCII.GetBytes(workload.ToString()));
                }

                Console.WriteLine("Total expected cost: {0} msec", total_msec);
                System.Threading.Thread.Sleep(1);  //  Give 0MQ time to deliver
            }
            Console.WriteLine("Hit a key to exit");
            Console.ReadKey();
        }
    }

}