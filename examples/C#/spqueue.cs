//
//  Simple Pirate queue
//  This is identical to the LRU pattern, with no reliability mechanisms
//  at all. It depends on the client for recovery. Runs forever.
//
//  Author: Kristian Kristensen <kristian@kristenseninc.com>
//  Based on lruqueue2 by Michael Compton, Tomas Roos

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.spqueue
{
    class Program
    {
        private const string LRU_READY = "READY";

        static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket frontend = context.CreateSocket(SocketType.ROUTER), backend = context.CreateSocket(SocketType.ROUTER))
                {
                    frontend.Bind("tcp://*:5555"); // For Clients
                    backend.Bind("tcp://*:5556"); // For Workers

                    //  Logic of LRU loop
                    //  - Poll backend always, frontend only if 1+ worker ready
                    //  - If worker replies, queue worker as ready and forward reply
                    //    to client if necessary
                    //  - If client requests, pop next worker and send request to it

                    //  Queue of available workers
                    var workerQueue = new Queue<byte[]>();

                    //  Handle worker activity on backend
                    backend.ReceiveReady += (s, e) =>
                                                 {
                                                     var zmsg = new ZMessage(e.Socket);
                                                     //  Use worker address for LRU routing
                                                     workerQueue.Enqueue(zmsg.Unwrap());

                                                     //  Forward message to client if it's not a READY
                                                     if (!Encoding.Unicode.GetString(zmsg.Address).Equals(LRU_READY))
                                                     {
                                                         zmsg.Send(frontend);
                                                     }
                                                 };

                    frontend.ReceiveReady += (s, e) =>
                                                  {
                                                      //  Now get next client request, route to next worker
                                                      //  Dequeue and drop the next worker address
                                                      var zmsg = new ZMessage(e.Socket);
                                                      zmsg.Wrap(workerQueue.Dequeue(), new byte[0]);
                                                      zmsg.Send(backend);
                                                  };

                    var poller = new Poller(new ZmqSocket[] { frontend, backend });

                    while (true)
                    {
                        //int rc = ZmqContext.Poller(workerQueue.Count > 0
                        //                            ? new List<ZmqSocket>(new ZmqSocket[] {frontend, backend})
                        //                            : new List<ZmqSocket>(new ZmqSocket[] {backend}));

                        int rc = poller.Poll();

                        if (rc == -1)
                        {
                            break;
                        }
                    }
                }
            }
        }
    }
}
