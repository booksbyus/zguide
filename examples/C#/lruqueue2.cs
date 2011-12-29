//
//  Least-recently used (LRU) queue device
//  Demonstrates use of the zmsg class
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
using ZMsg;

namespace lruqueue2 {
    class Program {

        //  Basic request-reply client using REQ socket
        //
        static void ClientTask() {
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.REQ)) {
                    ZHelpers.SetID(client, Encoding.Unicode);
                    client.Connect("tcp://localhost:5555");

                    //  Send request, get repl
                    client.Send("HELLO", Encoding.Unicode);
                    string reply = client.Recv(Encoding.Unicode);
                    Console.WriteLine("Client: {0}", reply);
                }
            }
        }

        //  Worker using REQ socket to do LRU routing
        //
        static void WorkerTask() {
            using (Context ctx = new Context(1)) {
                using (Socket worker = ctx.Socket(SocketType.REQ)) {
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5556");

                    //  Tell broker we're ready for work
                    worker.Send("READY", Encoding.Unicode);
                    
                    while (true) {
                        ZMessage zmsg = new ZMessage(worker);
                        Console.WriteLine("Worker: {0}", Encoding.Unicode.GetString(zmsg.Body));
                        zmsg.StringToBody("OK");
                        zmsg.Send(worker);
                    }
                }
            }
        }

        const int NBR_CLIENTS = 10;
        const int NBR_WORKERS = 3;

        static void Main(string[] args) {
            List<Thread> workers = new List<Thread>();
            List<Thread> clients = new List<Thread>();

            //  Prepare our context and sockets
            using (Context ctx = new Context(1)) {
                using (Socket frontend = ctx.Socket(SocketType.ROUTER),
                    backend = ctx.Socket(SocketType.ROUTER)) {

                    frontend.Bind("tcp://*:5555");
                    backend.Bind("tcp://*:5556");

                    int clientNbr;
                    for (clientNbr = 0; clientNbr < NBR_CLIENTS; clientNbr++) {
                        clients.Add(new Thread(ClientTask));
                        clients[clientNbr].Start();
                    }

                    for (int workerNbr = 0; workerNbr < NBR_WORKERS; workerNbr++) {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerNbr].Start();
                    }

                    //  Logic of LRU loop
                    //  - Poll backend always, frontend only if 1+ worker ready
                    //  - If worker replies, queue worker as ready and forward reply
                    //    to client if necessary
                    //  - If client requests, pop next worker and send request to it

                    //  Queue of available workers
                    Queue<byte[]> workerQueue = new Queue<byte[]>();

                    //  Handle worker activity on backend
                    backend.PollInHandler += (skt, revents) => {
                        ZMessage zmsg = new ZMessage(skt);
                        //  Use worker address for LRU routing
                        workerQueue.Enqueue(zmsg.Unwrap());

                        //  Forward message to client if it's not a READY
                        if (!Encoding.Unicode.GetString(zmsg.Address).Equals("READY")) {
                            zmsg.Send(frontend);
                            clientNbr--; //  Exit after N messages
                        }  
                    };
                    frontend.PollInHandler += (skt, revents) => {
                        //  Now get next client request, route to next worker
                        //  Dequeue and drop the next worker address
                        ZMessage zmsg = new ZMessage(skt);
                        zmsg.Wrap(workerQueue.Dequeue(), new byte[0]);
                        zmsg.Send(backend);
                    };


                    while (clientNbr > 0) { //  Exit after N messages
                        if (workerQueue.Count > 0) {
                            Context.Poller(new List<Socket>(new Socket[] { frontend, backend }));
                        } else {
                            Context.Poller(new List<Socket>(new Socket[] { backend }));
                        }
                    }
                }
            }
        }
    }
}
