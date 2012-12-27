//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Collections.Generic;
using System.Text;
using ZeroMQ;
using ZeroMQ.Interop;
using System.Threading;

namespace zguide.taskworker2 {
    class TaskWorker2 {
        private ZmqContext context;
        private ZmqSocket receiver;
        private ZmqSocket sender;
        private ZmqSocket controller;
        private bool killCommand;

        public TaskWorker2() {
            context = ZmqContext.Create();
            receiver = context.CreateSocket(SocketType.PULL);
            sender = context.CreateSocket(SocketType.PUSH);
            controller = context.CreateSocket(SocketType.SUB);
        }

        public void Process() {
            //  Socket to receive messages on
            receiver.Connect("tcp://localhost:5557");
            //  Socket to send messages to
            sender.Connect("tcp://localhost:5558");
            //  Socket for control input
            controller.Connect("tcp://localhost:5559");
            controller.Subscribe(Encoding.Unicode.GetBytes(string.Empty));

            //  Process messages from receiver and controller
            receiver.ReceiveReady += ReceiverPollInHandler;
            controller.ReceiveReady += ControllerPollInHandler;

            var poller = new Poller(new List<ZmqSocket> { receiver, controller });

            //  Process messages from both sockets
            killCommand = false;
            while (!killCommand) {
                poller.Poll();
            }
        }

        private void ReceiverPollInHandler(object s, SocketEventArgs e)
        {
            //  Process task
            int workload = Convert.ToInt32(e.Socket.Receive(Encoding.Unicode));
            //  Do the work
            Thread.Sleep(workload);
            //  Send results to sink
            sender.Send("", Encoding.Unicode);
            Console.WriteLine(".");
            Console.Clear();
        }

        private void ControllerPollInHandler(object s, SocketEventArgs e)
        {
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
