using System;
using System.Text;

/**
* Author: Eric Desgranges
* Email: eric@vcardprocessor.com
* License: This example code licensed under the MIT/X11 license.
*/

namespace Client
{
    static class Program
    {
        //
        //  Task worker
        //  Connects PULL socket to tcp://localhost:5557
        //  Collects workloads from ventilator via that socket
        //  Connects PUSH socket to tcp://localhost:5558
        //  Sends results to sink via that socket
        //
        public static void Start()
        {
            using (var context = new ZMQ.Context(1))
            {
                //  Socket to receive messages on
                var receiver = context.Socket(ZMQ.PULL);
                receiver.Connect("tcp://localhost:5557");

                //  Socket to send messages to
                var sender = context.Socket(ZMQ.PUSH);
                sender.Connect("tcp://localhost:5558");

                //  Process tasks forever
                while (true)
                {
                    byte[] bytes;
                    receiver.Recv(out bytes);
                    int milliseconds;
                    int.TryParse(Encoding.ASCII.GetString(bytes), out milliseconds);
                    //  Simple progress indicator for the viewer
                    Console.Write(new string('.', milliseconds / 10));

                    //  Do the work
                    System.Threading.Thread.Sleep(milliseconds);

                    //  Send results to sink
                    sender.Send(Encoding.ASCII.GetBytes(""));
                }
            }
        }
    }
}