//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Diagnostics;
using ZMQ;
using System.Threading;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            using (Context context = new Context(1)) {
                using (Socket receiver = context.Socket(SocketType.PULL),
                    controller = context.Socket(SocketType.PUB)) {
                    //  Socket to receive messages on
                    receiver.Bind("tcp://*:5558");
                    //  Socket for worker control
                    controller.Bind("tcp://*:5559");
                    
                    //  Wait for start of batch
                    receiver.Recv();

                    //  Start our clock now
                    Stopwatch stopwatch = new Stopwatch();
                    stopwatch.Start();
                    
                    //  Process 100 confirmations
                    int taskNbr = 0;
                    for (; taskNbr < 100; taskNbr++) {
                        receiver.Recv();
                        if ((taskNbr / 10) * 10 == taskNbr)
                            Console.WriteLine(":");
                        else
                            Console.WriteLine(".");
                    }
                    //  Calculate and report duration of batch
                    stopwatch.Stop();
                    Console.WriteLine("Total elapsed time: {0} msec", stopwatch.ElapsedMilliseconds);
                    
                    //  Send kill signal to workers
                    controller.Send("KILL", Encoding.Unicode);
                    Thread.Sleep(1000);     //  Give 0MQ time to deliver
                }
            }
        }
    }
}
