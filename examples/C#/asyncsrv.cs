﻿//
//  Asynchronous client-to-server (DEALER to ROUTER)
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each task has its own
//  context and conceptually acts as a separate process.

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZeroMQ;
using zguide;

namespace zguide.asycnsrv
{
    internal class Program
    {
        //  This main thread simply starts several clients, and a server, and then
        //  waits for the server to finish.
        public static void Main(string[] args)
        {
            var clients = new List<Thread>(3);
            for (int clientNumber = 0; clientNumber < 3; clientNumber++)
            {
                clients.Add(new Thread(ClientTask));
                clients[clientNumber].Start();
            }

            var serverThread = new Thread(ServerTask);
            serverThread.Start();

            Console.ReadLine();
        }

        //  ---------------------------------------------------------------------
        //  This is our client task
        //  It connects to the server, and then sends a request once per second
        //  It collects responses as they arrive, and it prints them out. We will
        //  run several client tasks in parallel, each with a different random ID.
        public static void ClientTask()
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket client = context.CreateSocket(SocketType.DEALER))
                {
                    //  Generate printable identity for the client
                    string identity = ZHelpers.SetID(client, Encoding.Unicode);
                    client.Connect("tcp://localhost:5570");

                    client.ReceiveReady += (s, e) =>
                    {
                        var zmsg = new ZMessage(e.Socket);
                        Console.WriteLine("{0} : {1}", identity, zmsg.BodyToString());
                    };

                    int requestNumber = 0;

                    var poller = new Poller(new List<ZmqSocket> { client });

                    while (true)
                    {
                        //  Tick once per second, pulling in arriving messages
                        for (int centitick = 0; centitick < 100; centitick++)
                        {
                            poller.Poll(TimeSpan.FromMilliseconds(10));
                        }
                        var zmsg = new ZMessage("");
                        zmsg.StringToBody(String.Format("request: {0}", ++requestNumber));
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
        private static void ServerTask()
        {
            var workers = new List<Thread>(5);
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket frontend = context.CreateSocket(SocketType.ROUTER), backend = context.CreateSocket(SocketType.DEALER))
                {
                    frontend.Bind("tcp://*:5570");
                    backend.Bind("inproc://backend");

                    for (int workerNumber = 0; workerNumber < 5; workerNumber++)
                    {
                        workers.Add(new Thread(ServerWorker));
                        workers[workerNumber].Start(context);
                    }

                    //  Switch messages between frontend and backend
                    frontend.ReceiveReady += (s, e) =>
                    {
                        var zmsg = new ZMessage(e.Socket);
                        zmsg.Send(backend);
                    };

                    backend.ReceiveReady += (s, e) =>
                    {
                        var zmsg = new ZMessage(e.Socket);
                        zmsg.Send(frontend);
                    };

                    var poller = new Poller(new List<ZmqSocket> {frontend, backend});

                    while (true)
                    {
                        poller.Poll();
                    }
                }
            }
        }

        //  Accept a request and reply with the same text a random number of
        //  times, with random delays between replies.
        private static void ServerWorker(object context)
        {
            var randomizer = new Random(DateTime.Now.Millisecond);
            using (ZmqSocket worker = ((ZmqContext)context).CreateSocket(SocketType.DEALER))
            {
                worker.Connect("inproc://backend");

                while (true)
                {
                    //  The DEALER socket gives us the address envelope and message
                    var zmsg = new ZMessage(worker);
                    //  Send 0..4 replies back
                    int replies = randomizer.Next(5);
                    for (int reply = 0; reply < replies; reply++)
                    {
                        Thread.Sleep(randomizer.Next(1, 1000));
                        zmsg.Send(worker);
                    }
                }
            }
        }
    }
}
