//
//  Durable subscriber
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
                using (Socket subscriber = context.Socket(SocketType.SUB), sync = context.Socket(SocketType.PUSH))
                {
                    //  Connect our subscriber socket
                    subscriber.StringToIdentity("Hello", Encoding.Unicode);
                    subscriber.Subscribe("", Encoding.Unicode);
                    subscriber.Connect("tcp://localhost:5565");

                    //  Synchronize with publisher
                    sync.Connect("tcp://localhost:5564");
                    sync.Send("", Encoding.Unicode);

                    string message = "";
                    while (!message.Equals("END"))
                    {
                        message = subscriber.Recv(Encoding.Unicode);
                        Console.WriteLine(message);
                    }
                }
            }
        }
    }
}
