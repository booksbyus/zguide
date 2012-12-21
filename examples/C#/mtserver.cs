//
//  Multithreaded Hello World server
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using System.Threading;
using ZeroMQ;

namespace ZMQGuide
{
    internal class Program9
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket clients = context.CreateSocket(SocketType.ROUTER), workers = context.CreateSocket(SocketType.DEALER))
                {
                    clients.Bind("tcp://*:5555");
                    workers.Bind("inproc://workers"); // FYI, inproc requires that bind is performed before connect

                    var workerThreads = new Thread[5];
                    for (int threadId = 0; threadId < workerThreads.Length; threadId++)
                    {
                        workerThreads[threadId] = new Thread(WorkerRoutine);
                        workerThreads[threadId].Start(context);
                    }

                    //  Connect work threads to client threads via a queue
                    //  Devices will be depricated from 3.x
                    ZeroMQ.Devices.QueueDevice(clients, workers);
                }
            }
        }

        private static void WorkerRoutine(object context)
        {
            ZmqSocket receiver = ((ZmqContext)context).CreateSocket(SocketType.REP);
            receiver.Connect("inproc://workers");

            while (true)
            {
                string message = receiver.Receive(Encoding.Unicode);

                Thread.Sleep(1000); //  Simulate 'work'
                
                receiver.Send("World", Encoding.Unicode);
            }
        }
    }
}
