//
//  Task worker
//  Connects PULL ZmqSocket to tcp://localhost:5557
//  Collects workloads from ventilator via that ZmqSocket
//  Connects PUSH ZmqSocket to tcp://localhost:5558
//  Sends results to sink via that ZmqSocket
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide {
    class Program30 {
        static void Main(string[] args) {
            //  Prepare our ZmqContext
            using (ZmqContext ZmqContext = ZmqContext.Create()) {
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
