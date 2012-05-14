//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket frontend = context.Socket(SocketType.ROUTER), backend = context.Socket(SocketType.DEALER))
                {
                    frontend.Bind("tcp://*:5559");
                    backend.Bind("tcp://*:5560");

                    //  Note that the Devices will be removed in the 3.x release.
                    //  This replaces the example in rrbroker.cs
                    Socket.Device.Queue(frontend, backend);
                }
            }
        }
    }
}
