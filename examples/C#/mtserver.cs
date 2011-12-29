//
//  Multithreaded Hello World server
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide {
    class Program {

        static void WorkerRoutine(object context) {
            //  Socket to talk to dispatcher
            Socket receiver = ((Context)context).Socket(SocketType.REP);
            receiver.Connect("inproc://workers");
            while (true) {
                string message = receiver.Recv(Encoding.Unicode);
                //  Do some 'work'
                Thread.Sleep(1000);
                //  Send reply back to client
                receiver.Send("World", Encoding.Unicode);
            }
        }

        static void Main(string[] args) {
            using (Context context = new Context(1)) {
                using (Socket clients = context.Socket(SocketType.ROUTER),
                workers = context.Socket(SocketType.DEALER)) {
                    //  Socket to talk to clients
                    clients.Bind("tcp://*:5555");
                    //  Socket to talk to workers
                    workers.Bind("inproc://workers");
                    
                    //  Launch pool of worker threads
                    Thread[] workerThreads = new Thread[5];
                    for (int count = 0; count < workerThreads.Length; count++) {
                        workerThreads[count] = new Thread(WorkerRoutine);
                        workerThreads[count].Start(context);
                    }

                    //  Connect work threads to client threads via a queue
                    Socket.Device.Queue(clients, workers);
                }
            }
        }
    }
}
