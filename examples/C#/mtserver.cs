//
//  Multithreaded Hello World server
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

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
                using (Socket clients = context.Socket(SocketType.ROUTER), workers = context.Socket(SocketType.DEALER))
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
                    Socket.Device.Queue(clients, workers);
                }
            }
        }

        private static void WorkerRoutine(object context)
        {
            Socket receiver = ((Context)context).Socket(SocketType.REP);
            receiver.Connect("inproc://workers");

            while (true)
            {
                string message = receiver.Recv(Encoding.Unicode);

                Thread.Sleep(1000); //  Simulate 'work'
                
                receiver.Send("World", Encoding.Unicode);
            }
        }
    }
}
