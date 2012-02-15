//
//  Task worker - design 2
//  Adds pub-sub flow to receive and respond to kill signal
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Diagnostics;
using ZMQ;

namespace ZMQGuide 
{
    internal class Program 
    {
        public static void Main(string[] args) 
        {
            using (var context = new Context(1)) 
            {
                using (Socket receiver = context.Socket(SocketType.PULL), controller = context.Socket(SocketType.PUB)) 
                {
                    receiver.Bind("tcp://*:5558");
                    controller.Bind("tcp://*:5559");
                    
                    //  Wait for start of batch
                    receiver.Recv();

                    var stopwatch = new Stopwatch();
                    stopwatch.Start();

                    const int tasksToConfirm = 100;
                    for (int taskNumber = 0; taskNumber < tasksToConfirm; taskNumber++)
                    {
                        string message = receiver.Recv(Encoding.Unicode);
                        Console.WriteLine(taskNumber % 10 == 0 ? ":" : ".");
                    }

                    stopwatch.Stop();
                    Console.WriteLine("Total elapsed time: {0}", stopwatch.ElapsedMilliseconds);

                    controller.Send("KILL", Encoding.Unicode);
                }
            }
        }
    }
}
