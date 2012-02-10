//
//  Weather proxy device
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

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
                using (Socket frontend = context.Socket(SocketType.SUB), backend = context.Socket(SocketType.PUB))
                {
                    //  This is where the weather server sits
                    frontend.Connect("tcp://127.0.0.1:5556");
                    frontend.Subscribe("", Encoding.Unicode);

                    //  This is our public endpoint for subscribers
                    backend.Bind("tcp://*:8100"); // i use local to be able to run the example, this could be the public ip instead eg. tcp://10.1.1.0:8100

                    //  Shunt messages out to our own subscribers
                    while (true)
                    {
                        bool hasMore = true;
                        while (hasMore)
                        {
                            string message = frontend.Recv(Encoding.Unicode);
                            hasMore = frontend.RcvMore;
                            backend.Send(message, Encoding.Unicode, hasMore ? SendRecvOpt.SNDMORE : SendRecvOpt.NONE);
                        }
                    }
                }
            }
        }
    }
}
