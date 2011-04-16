//
//  Task sink
//  Binds PULL socket to tcp://localhost:5558
//  Collects results from workers via that socket
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using System.Diagnostics;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            //  Prepare our context and socket
            using (Context context = new Context(1)) {
                using (Socket receiver = context.Socket(SocketType.PULL)) {
                    receiver.Bind("tcp://*:5558");
                    //  Wait for start of batch
                    receiver.Recv();
                    Stopwatch stopwatch = new Stopwatch();
                    stopwatch.Start();
                    //  Process 100 confirmations
                    for (int taskNbr = 0; taskNbr < 100; taskNbr++) {
                        string message = receiver.Recv(Encoding.Unicode);
                        if ((taskNbr / 10) * 10 == taskNbr)
                            Console.WriteLine(":");
                        else
                            Console.WriteLine(".");
                    }
                    //  Calculate and report duration of batch
                    stopwatch.Stop();
                    Console.WriteLine("Total elapsed time: {0}", stopwatch.ElapsedMilliseconds);
                }
            }
        }
    }
}
