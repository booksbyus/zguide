//
//  Multithreaded relay
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

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
                using (Socket socket = context.Socket(SocketType.PAIR))
                {
                    //  Bind to inproc: endpoint, then start upstream thread
                    socket.Bind("inproc://step3");

                    var step2 = new Thread(Step2);
                    step2.Start(context);

                    //  Wait for signal
                    socket.Recv();

                    Console.WriteLine("Test Successful!!!");
                }
            }
        }

        private static void Step2(object context)
        {
            //  Bind to inproc: endpoint, then start upstream thread
            using (Socket receiver = ((Context)context).Socket(SocketType.PAIR))
            {
                receiver.Bind("inproc://step2");

                var step1 = new Thread(Step1);
                step1.Start(context);

                //  Wait for signal
                receiver.Recv();
            }

            //  Signal downstream to step 3
            using (Socket sender = ((Context)context).Socket(SocketType.PAIR))
            {
                sender.Connect("inproc://step3");
                sender.Send("", Encoding.Unicode);
            }
        }

        private static void Step1(object context)
        {
            //  Signal downstream to step 2
            using (Socket sender = ((Context)context).Socket(SocketType.PAIR))
            {
                sender.Connect("inproc://step2");
                sender.Send("", Encoding.Unicode);
            }
        }
    }
}
