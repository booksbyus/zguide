//
//  Custom routing Router to Dealer
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Threading;
using System.Collections.Generic;
using System.Text;
using ZMQ;

namespace rtdealer {
    class Program {
        //  We have two workers, here we copy the code, normally these would
        //  run on different boxes...
        //
        static void WorkerTaskA() {
            using (Context ctx = new Context(1)) {
                using (Socket worker = ctx.Socket(SocketType.DEALER)) {
                    worker.StringToIdentity("A", Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");
                    int total = 0;
                    while (true) {
                        string request = worker.Recv(Encoding.Unicode);
                        if(request.Equals("END")) {
                            Console.WriteLine("A Received: {0}", total);
                            break;
                        }
                        total++;
                    }
                }
            }
        }

        static void WorkerTaskB() {
            using (Context ctx = new Context(1)) {
                using (Socket worker = ctx.Socket(SocketType.DEALER)) {
                    worker.StringToIdentity("B", Encoding.Unicode);
                    worker.Connect("tcp://localhost:5555");
                    int total = 0;
                    while (true) {
                        string request = worker.Recv(Encoding.Unicode);
                        if (request.Equals("END")) {
                            Console.WriteLine("B Received: {0}", total);
                            break;
                        }
                        total++;
                    }
                }
            }
        }

        static void Main(string[] args) {
            Random rand = new Random(DateTime.Now.Millisecond);
            List<Thread> workers = new List<Thread>(new Thread[] { 
                new Thread(WorkerTaskA), new Thread(WorkerTaskB) });
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.ROUTER)) {
                    client.Bind("tcp://*:5555");
                    foreach (Thread thread in workers) {
                        thread.Start();
                    }

                    //  Wait for threads to connect, since otherwise the messages
                    //  we send won't be routable.
                    Thread.Sleep(1000);

                    //  Send 10 tasks scattered to A twice as often as B
                    for (int taskNbr = 0; taskNbr < 10; taskNbr++) {
                        //  Send two message parts, first the address...
                        if (rand.Next(3) > 0) {
                            client.SendMore("A", Encoding.Unicode);
                        } else {
                            client.SendMore("B", Encoding.Unicode);
                        }
                        //  And then the workload
                        client.Send("This is the workload", Encoding.Unicode);
                    }

                    client.SendMore("A", Encoding.Unicode);
                    client.Send("END", Encoding.Unicode);

                    client.SendMore("B", Encoding.Unicode);
                    client.Send("END", Encoding.Unicode);
                    Console.ReadLine();
                }
            }
        }
    }
}
