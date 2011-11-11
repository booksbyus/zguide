//
//  Task ventilator
//  Binds PUSH socket to tcp://localhost:5557
//  Sends batch of tasks to workers via that socket
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            //  Prepare our context
            using (Context context = new Context(1)) {
                //  Socket to send messages on
                using (Socket sender = context.Socket(SocketType.PUSH)) {
                    sender.Bind("tcp://*:5557");

                    Console.WriteLine("Press enter when the workers are ready: ");
                    while (Console.ReadKey(true).Key != ConsoleKey.Enter) ;
                    Console.WriteLine("Sending tasks to workers...");

                    //  The first message is "0" and signals start of batch
                    sender.Send("0", Encoding.Unicode);

                    //  Initialize random number generator
                    Random rand = new Random(DateTime.Now.Millisecond);

                    //  Send 100 tasks
                    int totalMsec = 0;  //  Total expected cost in msecs
                    for (int taskNbr = 0; taskNbr < 100; taskNbr++) {
                        int workload;
                        //  Random workload from 1 to 100msecs
                        workload = rand.Next(1, 100);
                        totalMsec += workload;
                        sender.Send(workload.ToString(), Encoding.Unicode);
                    }
                    Console.WriteLine("Total expected cost: {0} msec", totalMsec);
                    Thread.Sleep(1000); //  Give 0MQ time to deliver
                }
            }
        }
    }
}
