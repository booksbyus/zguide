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
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket receiver = context.Socket(SocketType.PULL), sender = context.Socket(SocketType.PUSH), controller = context.Socket(SocketType.SUB))
                {
                    receiver.Connect("tcp://localhost:5557");
                    sender.Connect("tcp://localhost:5558");
                    controller.Connect("tcp://localhost:5559");
                    controller.Subscribe(string.Empty, Encoding.Unicode);

                    bool run = true;

                    var items = new PollItem[2];
                    items[0] = receiver.CreatePollItem(IOMultiPlex.POLLIN);
                    items[0].PollInHandler += (socket, revents) => ReceiverPollInHandler(socket, sender);
                    items[1] = controller.CreatePollItem(IOMultiPlex.POLLIN);
                    items[1].PollInHandler += delegate { run = false; };

                    //  Process tasks as long as the controller does not signal the end.
                    while (run)
                    {
                        context.Poll(items);
                    }
                }
            }
        }

        private static void ReceiverPollInHandler(Socket receiver, Socket sender)
        {
            string task = receiver.Recv(Encoding.Unicode);

            //  Simple progress indicator for the viewer;
            Console.WriteLine("{0}.", task);

            int sleepTime = Convert.ToInt32(task);
            Thread.Sleep(sleepTime);

            // Send 'result' to the sink
            sender.Send("", Encoding.Unicode);
        }
    }
}