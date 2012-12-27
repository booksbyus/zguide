//
//  Task worker - design 2
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//  Connects SUB socket to tcp://localhost:5559
//  And shuts down worker when it gets the kill signal
//

//  Author:     Mark Kharitonov, Tomas Roos
//  Email:      Mark.Kharitonov@shunra.co.il, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZeroMQ;
using ZeroMQ.Interop;

namespace zguide.taskwork2
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket receiver = context.CreateSocket(SocketType.PULL), sender = context.CreateSocket(SocketType.PUSH), controller = context.CreateSocket(SocketType.SUB))
                {
                    receiver.Connect("tcp://localhost:5557");
                    sender.Connect("tcp://localhost:5558");
                    controller.Connect("tcp://localhost:5559");
                    controller.SubscribeAll();

                    bool run = true;

                    var poller = new Poller(new List<ZmqSocket> { receiver, controller });

                    receiver.ReceiveReady += (s, e) => ReceiverPollInHandler(e.Socket, sender);
                    controller.ReceiveReady += delegate { run = false; };

                    //  Process tasks as long as the controller does not signal the end.
                    while (run)
                    {
                        poller.Poll();
                    }
                }
            }
        }

        private static void ReceiverPollInHandler(ZmqSocket receiver, ZmqSocket sender)
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