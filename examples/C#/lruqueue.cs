//
//  Least-recently used (LRU) queue device
//  Clients and workers are shown here in-process
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

namespace lruqueue {
    class Program {
        //  Basic request-reply client using REQ socket
        //
        static void ClientTask() {
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.REQ)) {
                    ZHelpers.SetID(client, Encoding.Unicode);
                    client.Connect("tcp://localhost:5555");
                    
                    //  Send request, get reply
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
                        //  Read and save all frames until we get an empty frame
                        //  In this example there is only 1 but it could be more
                        string address = worker.Recv(Encoding.Unicode);
                        string empty = worker.Recv(Encoding.Unicode);
                        
                        //  Get request, send reply
                        string request = worker.Recv(Encoding.Unicode);
                        Console.WriteLine("Worker: {0}", request);

                        worker.SendMore(address, Encoding.Unicode);
                        worker.SendMore();
                        worker.Send("OK", Encoding.Unicode);
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
                    Queue<string> workerQueue = new Queue<string>();

                    //  Handle worker activity on backend
                    backend.PollInHandler += (skt, revents) => {
                        //  Queue worker address for LRU routing
                        string workerAddr = skt.Recv(Encoding.Unicode);
                        workerQueue.Enqueue(workerAddr);

                        //  Second frame is empty
                        string empty = skt.Recv(Encoding.Unicode);

                        //  Third frame is READY or else a client reply address
                        string clientAddr = skt.Recv(Encoding.Unicode);

                        //  If client reply, send rest back to frontend
                        if (!clientAddr.Equals("READY")) {
                            empty = skt.Recv(Encoding.Unicode);
                            string reply = skt.Recv(Encoding.Unicode);
                            frontend.SendMore(clientAddr, Encoding.Unicode);
                            frontend.SendMore();
                            frontend.Send(reply, Encoding.Unicode);

                            clientNbr--; //  Exit after N messages
                        }
                    };
                    frontend.PollInHandler += (skt, revents) => {
                        //  Now get next client request, route to LRU worker
                        //  Client request is [address][empty][request]
                        string clientAddr = skt.Recv(Encoding.Unicode);
                        string empty = skt.Recv(Encoding.Unicode);
                        string request = skt.Recv(Encoding.Unicode);

                        backend.SendMore(workerQueue.Dequeue(), Encoding.Unicode);
                        backend.SendMore();
                        backend.SendMore(clientAddr, Encoding.Unicode);
                        backend.SendMore();
                        backend.Send(request, Encoding.Unicode);
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
