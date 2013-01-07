//
//  Task worker
//  Connects PULL socket to tcp://localhost:5557
//  Collects workloads from ventilator via that socket
//  Connects PUSH socket to tcp://localhost:5558
//  Sends results to sink via that socket
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.taskworker {
    class Program {
        static void Main(string[] args) {
            //  Prepare our context
            using (ZmqContext context = ZmqContext.Create())
            {
                //  Sockets to send and receive messages on
                using (ZmqSocket receiver = context.CreateSocket(SocketType.PULL),
                    sender = context.CreateSocket(SocketType.PUSH)) {
                    receiver.Connect("tcp://localhost:5557");
                    sender.Connect("tcp://localhost:5558");

                    //  Process tasks forever
                    while (true) {
                        string message = receiver.Receive(Encoding.Unicode);
                        //  Simple progress indicator for the viewer
                        Console.Clear();
                        Console.WriteLine("{0}.", message);
                        
                        //  Do the work
                        Thread.Sleep(Convert.ToInt32(message));
                        
                        //  Send results to sink
                        sender.Send("", Encoding.Unicode);
                    }
                }
            }
        }
    }
}
