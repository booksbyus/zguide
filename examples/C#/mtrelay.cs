//
//  Multithreaded relay
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program11
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket ZmqSocket = context.CreateSocket(SocketType.PAIR))
                {
                    //  Bind to inproc: endpoint, then start upstream thread
                    ZmqSocket.Bind("inproc://step3");

                    var step2 = new Thread(Step2);
                    step2.Start(context);

                    //  Wait for signal
                    ZmqSocket.Receive();

                    Console.WriteLine("Test Successful!!!");
                }
            }
        }

        private static void Step2(object context)
        {
            //  Bind to inproc: endpoint, then start upstream thread
            using (ZmqSocket receiver = ((ZmqContext)context).CreateSocket(SocketType.PAIR))
            {
                receiver.Bind("inproc://step2");

                var step1 = new Thread(Step1);
                step1.Start(context);

                //  Wait for signal
                receiver.Receive();
            }

            //  Signal downstream to step 3
            using (ZmqSocket sender = ((ZmqContext)context).CreateSocket(SocketType.PAIR))
            {
                sender.Connect("inproc://step3");
                sender.Send("", Encoding.Unicode);
            }
        }

        private static void Step1(object ZmqContext)
        {
            //  Signal downstream to step 2
            using (ZmqSocket sender = ((ZmqContext)context).CreateSocket(SocketType.PAIR))
            {
                sender.Connect("inproc://step2");
                sender.Send("", Encoding.Unicode);
            }
        }
    }
}
