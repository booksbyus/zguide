//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using ZeroMQ;

namespace zguide.msqueue
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (var queue = new ZeroMQ.Devices.QueueDevice(context, "tcp://*:5559", "tcp://*:5560"))
                {
                    
                }
            }
        }
    }
}
