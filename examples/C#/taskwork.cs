//  Task worker
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//


//  Author:     Mike Sheridan, Tomas Roos
//  Email:      mike@westforkconsulting.com, ptomasroos@gmail.com

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
            var worker = new Worker();
            worker.ProcessTasks();
        }
    }

    internal class Worker
    {
        public void ProcessTasks()
        {
            using (var context = new Context(1))
            {
                //  Sockets to receive tasks on and send messages to sink
                using (Socket receiver = context.Socket(SocketType.PULL), sender = context.Socket(SocketType.PUSH))
                {
                    receiver.Connect("tcp://localhost:5557");
                    sender.Connect("tcp://localhost:5558");

                    //  Process tasks forever
                    while (true)
                    {
                        string task = receiver.Recv(Encoding.Unicode);
                        int sleepTime = Convert.ToInt32(task);

                        //  Simple progress indicator for the viewer;
                        Console.WriteLine("{0}.", task);

                        //  Do the work
                        Thread.Sleep(sleepTime);

                        // Send result to the sink
                        sender.Send("", Encoding.Unicode);
                    }
                }
            }
        }
    }
}