//
//  Publisher for durable subscriber
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket sync = context.Socket(SocketType.PULL), publisher = context.Socket(SocketType.PUB))
                {
                    sync.Bind("tcp://*:5564");

                    publisher.HWM = 1;
                    //  Specify swap space in bytes, this covers all subscribers
                    publisher.Swap = 25000000;
                    publisher.Bind("tcp://*:5565");

                    //  Wait for synchronization request
                    sync.Recv();

                    for (int updateNumber = 0; updateNumber < 10; updateNumber++)
                    {
                        publisher.Send("Update " + updateNumber, Encoding.Unicode);
                        Thread.Sleep(1000);
                    }

                    publisher.Send("END", Encoding.Unicode);
                }
            }
        }
    }
}
