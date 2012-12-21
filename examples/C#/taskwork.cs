//
//  Task worker
//  Connects PULL ZmqSocket to tcp://localhost:5557
//  Collects workloads from ventilator via that ZmqSocket
//  Connects PUSH ZmqSocket to tcp://localhost:5558
//  Sends results to sink via that ZmqSocket
//

//  Author:     Mike Sheridan, Tomas Roos
//  Email:      mike@westforkconsulting.com, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program32
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket receiver = context.CreateSocket(SocketType.PULL), sender = context.CreateSocket(SocketType.PUSH))
                {
                    receiver.Connect("tcp://localhost:5557");
                    sender.Connect("tcp://localhost:5558");

                    while (true)
                    {
                        string task = receiver.Receive(Encoding.Unicode);

                        //  Simple progress indicator for the viewer;
                        Console.WriteLine("{0}.", task);

                        int sleepTime = Convert.ToInt32(task);
                        Thread.Sleep(sleepTime);

                        // Send 'result' to the sink
                        sender.Send("", Encoding.Unicode);
                    }
                }
            }
        }
    }
}