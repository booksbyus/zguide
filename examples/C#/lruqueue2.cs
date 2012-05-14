//
//  Least-recently used (LRU) queue device
//  Demonstrates use of the zmsg class
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

        private static void ClientTask()
        {
            using (var ctx = new Context(1))
            {
                using (var client = ctx.Socket(SocketType.REQ))
                {
                    while (true)
                    {
                        ZHelpers.SetID(client, Encoding.Unicode);
                        client.Connect("tcp://localhost:5555");

                        //  Send request, get repl
                        client.Send("HELLO", Encoding.Unicode);
                        string reply = client.Recv(Encoding.Unicode);

                        if (string.IsNullOrEmpty(reply))
                        {
                            break;
                        }

                        Console.WriteLine("Client: {0}", reply);
                    }
                }
            }
        }

        const int clients = 10;
        const int workers = 3;

        public static void Main(string[] args)
        {
            var workers = new List<Thread>();
            var clients = new List<Thread>();

            //  Prepare our context and sockets
            using (var ctx = new Context(1))
            {
                using (Socket frontend = ctx.Socket(SocketType.ROUTER), backend = ctx.Socket(SocketType.ROUTER))
                {
                    frontend.Bind("tcp://*:5555");
                    backend.Bind("tcp://*:5556");

                    int clientId;
                    for (clientId = 0; clientId < Program.clients; clientId++)
                    {
                        clients.Add(new Thread(ClientTask));
                        clients[clientId].Start();
                    }

                    for (int workerId = 0; workerId < Program.workers; workerId++)
                    {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerId].Start();
                    }

                    //  Logic of LRU loop
                    //  - Poll backend always, frontend only if 1+ worker ready
                    //  - If worker replies, queue worker as ready and forward reply
                    //    to client if necessary
                    //  - If client requests, pop next worker and send request to it

                    //  Queue of available workers
                    var workerQueue = new Queue<byte[]>();

                    //  Handle worker activity on backend
                    backend.PollInHandler += (socket, revents) =>
                                                 {
                                                     var zmsg = new ZMessage(socket);
                                                     //  Use worker address for LRU routing
                                                     workerQueue.Enqueue(zmsg.Unwrap());

                                                     //  Forward message to client if it's not a READY
                                                     if (!Encoding.Unicode.GetString(zmsg.Address).Equals("READY"))
                                                     {
                                                         zmsg.Send(frontend);
                                                     }
                                                 };

                    frontend.PollInHandler += (socket, revents) =>
                                                  {
                                                      //  Now get next client request, route to next worker
                                                      //  Dequeue and drop the next worker address
                                                      var zmsg = new ZMessage(socket);
                                                      zmsg.Wrap(workerQueue.Dequeue(), new byte[0]);
                                                      zmsg.Send(backend);
                                                  };

                    while (true)
                    {
                        int rc = Context.Poller(workerQueue.Count > 0
                                           ? new List<Socket>(new Socket[] {frontend, backend})
                                           : new List<Socket>(new Socket[] {backend}));

                        if (rc == -1)
                        {
                            break;
                        }
                    }
                }
            }
        }


        private static void WorkerTask()
        {
            using (var ctx = new Context(1))
            {
                using (var worker = ctx.Socket(SocketType.REQ))
                {
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect("tcp://localhost:5556");

                    //  Tell broker we're ready for work
                    worker.Send("READY", Encoding.Unicode);

                    while (true)
                    {
                        var zmsg = new ZMessage(worker);
                        Console.WriteLine("Worker: {0}", Encoding.Unicode.GetString(zmsg.Body));
                        zmsg.StringToBody("OK");
                        zmsg.Send(worker);
                    }
                }
            }
        }
    }
}
