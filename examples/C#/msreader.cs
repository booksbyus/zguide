//
//  Reading from multiple sockets
//  This version uses a simple recv loop
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program20
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                //  Connect to task ventilator and weather server
                using (ZmqSocket receiver = context.CreateSocket(SocketType.PULL), subscriber = context.CreateSocket(SocketType.SUB))
                {
                    receiver.Connect("tcp://localhost:5557");
                    subscriber.Connect("tcp://localhost:5556");
                    subscriber.Subscribe("10001 ", Encoding.Unicode);

                    //  Process messages from both sockets
                    //  We prioritize traffic from the task ventilator
                    while (true)
                    {
                        //  Process any waiting tasks
                        while (true)
                        {
                            byte[] message = receiver.Receive(SendRecvOpt.NOBLOCK);
                            if (message != null)
                            {
                                Console.WriteLine("Process Task");
                            }
                            else
                            {
                                break;
                            }
                        }

                        //  Process any waiting weather updates
                        while (true)
                        {
                            byte[] message = subscriber.Receive(SendRecvOpt.NOBLOCK);
                            if (message != null)
                            {
                                Console.WriteLine("Process Weather");
                            }
                            else
                            {
                                break;
                            }
                        }

                        Thread.Sleep(1000);
                    }
                }
            }
        }
    }
}
