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

namespace zguide.msreader
{
    internal class Program
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
                    subscriber.Subscribe(Encoding.Unicode.GetBytes("10001 "));

                    //  Process messages from both sockets
                    //  We prioritize traffic from the task ventilator
                    while (true)
                    {
                        //  Process any waiting tasks
                        while (true)
                        {
                            var message = new byte[0];
                            receiver.Receive(message, SocketFlags.DontWait);
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
                            var message = new byte[0];
                            subscriber.Receive(message, SocketFlags.DontWait);
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
