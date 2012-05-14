//
//  Reading from multiple sockets
//  This version uses zmq_poll(). 
//  This version is aims to be non-blocking, no Thread.Sleep here!
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
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

                    var items = new PollItem[2];
                    items[0] = receiver.CreatePollItem(IOMultiPlex.POLLIN);
                    items[0].PollInHandler += ReceiverPollInHandler;
                    items[1] = subscriber.CreatePollItem(IOMultiPlex.POLLIN);
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
        public static void ReceiverPollInHandler(Socket socket, IOMultiPlex revents)
        {
            socket.Recv();
            Console.WriteLine("Process Task");
        }

        // Weather server event
        public static void SubscriberPollInHandler(Socket socket, IOMultiPlex revents)
        {
            socket.Recv();
            Console.WriteLine("Process Weather");
        }
    }
}
