//
//  Multithreaded Hello World server
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System.Text;
using System.Threading;
using ZeroMQ;
using ZeroMQ.Devices;

namespace zguide.mtserver
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {

                using (var queue = new ZeroMQ.Devices.QueueDevice(context, "tcp://*:5555", "inproc://workers", DeviceMode.Blocking))
                {
                    queue.Initialize();
                    var workerThreads = new Thread[5];
                    for (int threadId = 0; threadId < workerThreads.Length; threadId++)
                    {
                        workerThreads[threadId] = new Thread(WorkerRoutine);
                        workerThreads[threadId].Start(context);
                    }
                    queue.Start();
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
