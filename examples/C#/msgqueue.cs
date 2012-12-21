//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using ZeroMQ;

namespace ZMQGuide
{
    internal class Program2
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket frontend = context.CreateSocket(SocketType.ROUTER), backend = context.CreateSocket(SocketType.DEALER))
                {
                    frontend.Bind("tcp://*:5559");
                    backend.Bind("tcp://*:5560");

                    //  Note that the Devices will be removed in the 3.x release.
                    //  This replaces the example in rrbroker.cs
                    ZeroMQ.Devices.QueueDevice.Queue(frontend, backend);
                }
            }
        }
    }
}
