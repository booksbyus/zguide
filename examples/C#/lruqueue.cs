//
//  Least-recently used (LRU) queue device
//  Clients and workers are shown here in-process
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using ZMQ;
using System.Threading;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            const int workersToStart = 3;
            int clientsRunning = 10;

            var workers = new List<Thread>();
            var clients = new List<Thread>();

            using (var context = new Context(1))
            {
                using (Socket frontend = context.Socket(SocketType.ROUTER), backend = context.Socket(SocketType.ROUTER))
                {
                    frontend.Bind("tcp://*:5555");
                    backend.Bind("tcp://*:5556");

                    for (int clientNumber = 0; clientNumber < clientsRunning; clientNumber++)
                    {
                        clients.Add(new Thread(ClientTask));
                        clients[clientNumber].Start();
                    }

                    for (int workerNumber = 0; workerNumber < workersToStart; workerNumber++)
                    {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerNumber].Start();
                    }

                    //  Logic of LRU loop
                    //  - Poll backend always, frontend only if 1+ worker ready
                    //  - If worker replies, queue worker as ready and forward reply
                    //    to client if necessary
                    //  - If client requests, pop next worker and send request to it
                    var workerQueue = new Queue<string>();

                    //  Handle worker activity on backend
                    backend.PollInHandler += (socket, revents) =>
                    {
                        //  Queue worker address for LRU routing
                        string workerAddress = socket.Recv(Encoding.Unicode);
                        workerQueue.Enqueue(workerAddress);

                        //  Second frame is empty
                        string empty = socket.Recv(Encoding.Unicode);

                        //  Third frame is READY or else a client reply address
                        string clientAddress = socket.Recv(Encoding.Unicode);

                        //  If client reply, send rest back to frontend
                        if (!clientAddress.Equals("READY"))
                        {
                            empty = socket.Recv(Encoding.Unicode);
                            string reply = socket.Recv(Encoding.Unicode);
                            frontend.SendMore(clientAddress, Encoding.Unicode);
                            frontend.SendMore();
                            frontend.Send(reply, Encoding.Unicode);

                            clientsRunning--; //  Exit after N messages
                        }
                    };

                    frontend.PollInHandler += (socket, revents) =>
                    {
                        //  Now get next client request, route to LRU worker
                        //  Client request is [address][empty][request]
                        string clientAddr = socket.Recv(Encoding.Unicode);
                        string empty = socket.Recv(Encoding.Unicode);
                        string request = socket.Recv(Encoding.Unicode);

                        backend.SendMore(workerQueue.Dequeue(), Encoding.Unicode);
                        backend.SendMore();
                        backend.SendMore(clientAddr, Encoding.Unicode);
                        backend.SendMore();
                        backend.Send(request, Encoding.Unicode);
                    };


                    while (clientsRunning > 0)
                    { //  Exit after N messages
                        Context.Poller(workerQueue.Count > 0
                                           ? new List<Socket>(new Socket[] {frontend, backend})
                                           : new List<Socket>(new Socket[] {backend}));
                    }
                }
            }
        }

        private static void ClientTask()
        {
            using (var context = new Context(1))
            {
                using (Socket client = context.Socket(SocketType.REQ))
                {
                    ZHelpers.SetID(client, Encoding.Unicode);
                    client.Connect("tcp://localhost:5555");

                    //  Send request, get reply
                    client.Send("HELLO", Encoding.Unicode);
                    string reply = client.Recv(Encoding.Unicode);
                    Console.WriteLine("Client: {0}", reply);
                }
            }
        }

        private static void WorkerTask()
        {
            using (var context = new Context(1))
            {
                using (Socket worker = context.Socket(SocketType.REQ))
                {
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5556");

                    //  Tell broker we're ready for work
                    worker.Send("READY", Encoding.Unicode);

                    while (true)
                    {
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
    }
}
