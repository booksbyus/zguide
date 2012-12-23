//
//  Reading from multiple sockets
//  This version uses zmq_poll(). 
//  This version is aims to be non-blocking, no Thread.Sleep here!
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;

namespace zguide.mspoller
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
                    subscriber.Subscribe("10001 ", Encoding.Unicode);

                    var items = new PollItem[2];
                    items[0] = receiver.CreatePollItem(Poller.POLLIN);
                    items[0].PollInHandler += ReceiverPollInHandler;
                    items[1] = subscriber.CreatePollItem(Poller.POLLIN);
                    items[1].PollInHandler += SubscriberPollInHandler;
                    
                    //  Process messages from both sockets
                    while (true)
                    {
                        context.Poll(items, -1);
                    }
                }
            }
        }

        // Task Processing event
        public static void ReceiverPollInHandler(ZmqSocket socket, Poller revents)
        {
            socket.Receive();
            Console.WriteLine("Process Task");
        }

        // Weather server event
        public static void SubscriberPollInHandler(ZmqSocket socket, Poller revents)
        {
            socket.Receive();
            Console.WriteLine("Process Weather");
        }
    }
}
