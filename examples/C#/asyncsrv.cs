//
//  Asynchronous client-to-server (XREQ to XREP)
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each task has its own
//  context and conceptually acts as a separate process.

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZMsg;
using ZMQ;

namespace asyncsrv {
    class Program {
        //  ---------------------------------------------------------------------
        //  This is our client task
        //  It connects to the server, and then sends a request once per second
        //  It collects responses as they arrive, and it prints them out. We will
        //  run several client tasks in parallel, each with a different random ID.
        static void ClientTask() {
            using (Context ctx = new Context(1)) {
                using (Socket client = ctx.Socket(SocketType.XREQ)) {
                    //  Generate printable identity for the client
                    ZHelpers.SetID(client, Encoding.Unicode);
                    string identity = client.IdentityToString(Encoding.Unicode);
                    client.Connect("tcp://localhost:5570");

                    client.PollInHandler += (skt, revents) => {
                        ZMessage zmsg = new ZMessage(skt);
                        Console.WriteLine("{0} : {1}", identity, zmsg.BodyToString());
                    };

                    int requestNbr = 0;
                    while (true) {
                        //  Tick once per second, pulling in arriving messages
                        for (int centitick = 0; centitick < 100; centitick++) {
                            Context.Poller(new List<Socket>(new Socket[] { client }), 10000);
                        }
                        ZMessage zmsg = new ZMessage("");
                        zmsg.StringToBody(String.Format("request: {0}", ++requestNbr));
                        zmsg.Send(client);
                    }
                }
            }
        }

        //  ---------------------------------------------------------------------
        //  This is our server task
        //  It uses the multithreaded server model to deal requests out to a pool
        //  of workers and route replies back to clients. One worker can handle
        //  one request at a time but one client can talk to multiple workers at
        //  once.
        static void ServerTask() {
            List<Thread> workers = new List<Thread>(5);
            using (Context ctx = new Context(1)) {
                using (Socket frontend = ctx.Socket(SocketType.XREP),
                    backend = ctx.Socket(SocketType.XREQ)) {
                    //  Frontend socket talks to clients over TCP
                    frontend.Bind("tcp://*:5570");
                    //  Backend socket talks to workers over inproc
                    backend.Bind("inproc://backend");

                    //  Launch pool of worker threads, precise number is not critical
                    for (int threadNbr = 0; threadNbr < 5; threadNbr++) {
                        workers.Add(new Thread(ServerWorker));
                        workers[threadNbr].Start(ctx);
                    }

                    //  Connect backend to frontend via a queue device
                    //  We could do this:
                    //      zmq_device (ZMQ_QUEUE, frontend, backend);
                    //  But doing it ourselves means we can debug this more easily

                    //  Switch messages between frontend and backend
                    frontend.PollInHandler += (skt, revents) => {
                        ZMessage zmsg = new ZMessage(skt);
                        zmsg.Send(backend);
                    };

                    backend.PollInHandler += (skt, revents) => {
                        ZMessage zmsg = new ZMessage(skt);
                        zmsg.Send(frontend);
                    };
                    List<Socket> sockets = new List<Socket>();
                    sockets.Add(frontend);
                    sockets.Add(backend);
                    while(true){
                        Context.Poller(sockets);
                    }
                }
            }
        }

        //  Accept a request and reply with the same text a random number of
        //  times, with random delays between replies.
        //
        static void ServerWorker(object ctx) {
            Random rand = new Random(DateTime.Now.Millisecond);
            using (Socket worker = ((Context)ctx).Socket(SocketType.XREQ)) {
                worker.Connect("inproc://backend");
                while (true) {
                    //  The XREQ socket gives us the address envelope and message
                    ZMessage zmsg = new ZMessage(worker);
                    //  Send 0..4 replies back
                    int replies = rand.Next(5);
                    for (int reply = 0; reply < replies; reply++) {
                        Thread.Sleep(rand.Next(1, 1000));
                        zmsg.Send(worker);
                    }
                }
            }
        }

        //  This main thread simply starts several clients, and a server, and then
        //  waits for the server to finish.
        //
        static void Main(string[] args) {
            ZHelpers.VersionAssert(2, 1);

            List<Thread> clients = new List<Thread>(3);
            for (int clientNbr = 0; clientNbr < 3; clientNbr++) {
                clients.Add(new Thread(ClientTask));
                clients[clientNbr].Start();
            }

            Thread serverThread = new Thread(ServerTask);
            serverThread.Start();

            Console.ReadLine();
        }
    }
}
