//
//  Weather proxy device
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using ZeroMQ;

namespace zguide.wuproxy
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket frontend = context.CreateSocket(SocketType.SUB), backend = context.CreateSocket(SocketType.PUB))
                {
                    //  This is where the weather server sits
                    frontend.Connect("tcp://127.0.0.1:5556");
                    frontend.Subscribe(new byte[0]);

                    //  This is our public endpoint for subscribers
                    backend.Bind("tcp://*:8100"); // i use local to be able to run the example, this could be the public ip instead eg. tcp://10.1.1.0:8100

                    //  Shunt messages out to our own subscribers
                    while (true)
                    {
                        bool hasMore = true;
                        while (hasMore)
                        {
                            string message = frontend.Receive(Encoding.Unicode);
                            hasMore = frontend.ReceiveMore;
                            backend.Send(Encoding.Unicode.GetBytes(message), message.Length, hasMore ? SocketFlags.SendMore : SocketFlags.None);
                        }
                    }
                }
            }
        }
    }
}
