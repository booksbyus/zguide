//
//  Reading from multiple sockets
//  This version uses a simple recv loop
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
                //  Connect to task ventilator and weather server
                using (Socket receiver = context.Socket(SocketType.PULL), subscriber = context.Socket(SocketType.SUB))
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
                            byte[] message = receiver.Recv(SendRecvOpt.NOBLOCK);
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
                            byte[] message = subscriber.Recv(SendRecvOpt.NOBLOCK);
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
