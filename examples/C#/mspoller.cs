//
//  Reading from multiple sockets
//  This version uses zmq_poll(). 
//  This version is aims to be non-blocking, no Thread.Sleep here!
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
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
                    subscriber.Subscribe(Encoding.Unicode.GetBytes("10001 "));

                    receiver.ReceiveReady += ReceiverPollInHandler;
                    subscriber.ReceiveReady += SubscriberPollInHandler;

                    var poller = new Poller(new List<ZmqSocket> {  });
                    
                    //  Process messages from both sockets
                    while (true)
                    {
                        poller.Poll();
                    }
                }
            }
        }

        // Task Processing event
        public static void ReceiverPollInHandler(object sender, SocketEventArgs e)
        {
            e.Socket.Receive(Encoding.Unicode);
            Console.WriteLine("Process Task");
        }

        // Weather server event
        public static void SubscriberPollInHandler(object sender, SocketEventArgs e)
        {
            e.Socket.Receive(Encoding.Unicode);
            Console.WriteLine("Process Weather");
        }
    }
}
