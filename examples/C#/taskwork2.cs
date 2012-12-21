//
//  Task worker - design 2
//  Connects PULL ZmqSocket to tcp://localhost:5557
//  Collects workloads from ventilator via that ZmqSocket
//  Connects PUSH ZmqSocket to tcp://localhost:5558
//  Sends results to sink via that ZmqSocket
//  Connects SUB ZmqSocket to tcp://localhost:5559
//  And shuts down worker when it gets the kill signal
//

//  Author:     Mark Kharitonov, Tomas Roos
//  Email:      Mark.Kharitonov@shunra.co.il, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program35
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
                    controller.Subscribe(string.Empty, Encoding.Unicode);

                    bool run = true;

                    var items = new PollItem[2];
                    items[0] = receiver.CreatePollItem(IOMultiPlex.POLLIN);
                    items[0].PollInHandler += (ZmqSocket, revents) => ReceiverPollInHandler(ZmqSocket, sender);
                    items[1] = controller.CreatePollItem(IOMultiPlex.POLLIN);
                    items[1].PollInHandler += delegate { run = false; };

                    //  Process tasks as long as the controller does not signal the end.
                    while (run)
                    {
                        ZmqContext.Poll(items);
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