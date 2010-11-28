//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;
using System.Threading;

namespace ZMQGuide {
    class TaskWorker2 {
        private Context context;
        private Socket receiver;
        private Socket sender;
        private Socket controller;
        private bool killCommand;

        public TaskWorker2() {
            context = new Context(1);
            receiver = context.Socket(SocketType.PULL);
            sender = context.Socket(SocketType.PUSH);
            controller = context.Socket(SocketType.SUB);
        }

        public void Process() {
            //  Socket to receive messages on
            receiver.Connect("tcp://localhost:5557");
            //  Socket to send messages to
            sender.Connect("tcp://localhost:5558");
            //  Socket for control input
            controller.Connect("tcp://localhost:5559");
            controller.Subscribe("", Encoding.Unicode);

            //  Process messages from receiver and controller
            PollItem[] items = new PollItem[2];
            items[0] = receiver.CreatePollItem(IOMultiPlex.POLLIN);
            items[0].PollInHandler += new PollHandler(ReceiverPollInHandler);
            items[1] = controller.CreatePollItem(IOMultiPlex.POLLIN);
            items[1].PollInHandler += new PollHandler(ControllerPollInHandler);

            //  Process messages from both sockets
            killCommand = false;
            while (!killCommand) {
                context.Poll(items, -1);
            }
        }

        private void ReceiverPollInHandler(Socket socket, IOMultiPlex revents) {
            //  Process task
            int workload = Convert.ToInt32(socket.Recv(Encoding.Unicode));
            //  Do the work
            Thread.Sleep(workload);
            //  Send results to sink
            sender.Send("", Encoding.Unicode);
            Console.WriteLine(".");
            Console.Clear();
        }

        private void ControllerPollInHandler(Socket socket, IOMultiPlex revents) {
            //  Any waiting controller command acts as 'KILL'
            Console.WriteLine("Killed...");
            killCommand = true;
        }
    }

    class Program {
        static void Main(string[] args) {
            TaskWorker2 taskworker = new TaskWorker2();
            taskworker.Process();
        }
    }
}
