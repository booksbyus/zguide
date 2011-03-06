//
//  Custom routing Router to Mama (XREP to REQ)
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Collections.Generic;
using System.Text;
using ZMQ;
using System.Threading;

namespace rtmama {    
    class Program {
        static void WorkerTask() {
            Random rand = new Random(DateTime.Now.Millisecond);
            using (Context ctx = new Context(1)) {
                using (Socket worker = ctx.Socket(SocketType.REQ)) {
                    //  We use a string identity for ease here
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");                    
                    int total = 0;
                    while (true) {
                        //  Tell the router we're ready for work
                        worker.Send("Ready", Encoding.Unicode);

                        //  Get workload from router, until finished
                        string workload = worker.Recv(Encoding.Unicode);
                        if (workload.Equals("END")) {
                            Console.WriteLine("Processed: {0} tasks", total);
                            break;
                        }
                        total++;

                        //  Do some random work
                        Thread.Sleep(rand.Next(1, 1000));
                    }
                }
            }
        }

        const int NBR_WORKERS = 10;

        static void Main(string[] args) {
            ZHelpers.VersionAssert(2, 1);
            List<Thread> workers = new List<Thread>(NBR_WORKERS);
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.XREP)) {
                    client.Bind("tcp://*:5555");
                    for (int workerNbr = 0; workerNbr < NBR_WORKERS; workerNbr++) {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerNbr].Start();
                    }

                    for (int taskNbr = 0; taskNbr < NBR_WORKERS * 10; taskNbr++) {
                        //  LRU worker is next waiting in queue
                        string address = client.Recv(Encoding.Unicode);
                        string empty = client.Recv(Encoding.Unicode);
                        string ready = client.Recv(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore();
                        client.Send("This is the workload", Encoding.Unicode);
                    }

                    //  Now ask mamas to shut down and report their results
                    for (int taskNbr = 0; taskNbr < NBR_WORKERS; taskNbr++) {
                        string address = client.Recv(Encoding.Unicode);
                        string empty = client.Recv(Encoding.Unicode);
                        string ready = client.Recv(Encoding.Unicode);

                        client.SendMore(address, Encoding.Unicode);
                        client.SendMore();
                        client.Send("END", Encoding.Unicode);
                    }
                    Console.ReadLine();
                }
            }
        }
    }
}
