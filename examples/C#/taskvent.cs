//
//  Task ventilator
//  Binds PUSH socket to tcp://localhost:5557
//  Sends batch of tasks to workers via that socket
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

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
            var ventilator = new Ventilator();
            ventilator.SendTasks();
            Console.ReadKey();
        }
    }

    internal class Ventilator
    {
        public void SendTasks()
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

                    //  Initialize random number generator
                    var randomizer = new Random(DateTime.Now.Millisecond);

                    //  Send 100 tasks
                    const int tasksToSend = 100;

                    int totalMsec = 0;  //  Total expected cost in msecs
                    
                    for (int taskNumber = 0; taskNumber < tasksToSend; taskNumber++)
                    {
                        //  Random workload from 1 to 100msecs
                        int workload = randomizer.Next(1, 100);
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
