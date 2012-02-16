//
//  Broker peering simulation (part 2) in C#
//  Prototypes the request-reply flow
//
//  While this example runs in a single process, that is just to make
//  it easier to start and stop the example. Each thread has its own
//  context and conceptually acts as a separate process.
//
//  Note! ipc doesnt work on windows and therefore type peering2 801 802 803

//  Author:     Tomas Roos
//  Email:      ptomasroos@gmail.com

using System;
using System.Linq;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        private const int numberOfClients = 10;
        private const int numberOfWorkers = 3;
        private static string cloudFeAddress;
        private static string localBeAddress;
        private static string localFeAddress;
        private static readonly Random randomizer = new Random(DateTime.Now.Millisecond);
        private static List<string> peers = new List<string>();

        public static void Main(string[] args)
        {
            if (args.Length < 2)
            {
                Console.WriteLine("Usage: peering2 <myself> <peer_1> … <peer_N>");
                return;
            }

            var myself = args[0];
            Console.WriteLine("Hello, I am " + myself);

            using (var context = new Context(1))
            {
                using (Socket cloudfe = context.Socket(SocketType.ROUTER), cloudbe = context.Socket(SocketType.ROUTER),
                    localfe = context.Socket(SocketType.ROUTER), localbe = context.Socket(SocketType.ROUTER))
                {
                    cloudFeAddress = "tcp://127.0.0.1:" + myself;
                    cloudfe.Identity = Encoding.Unicode.GetBytes(myself);
                    cloudfe.Bind(cloudFeAddress);

                    cloudbe.Identity = Encoding.Unicode.GetBytes(myself);
                    for (int arg = 1; arg < args.Length; arg++)
                    {
                        var endpoint = "tcp://127.0.0.1:" + args[arg];
                        peers.Add(endpoint);
                        Console.WriteLine("I: connecting to cloud frontend at " + endpoint);
                        cloudbe.Connect(endpoint);
                    }

                    localFeAddress = cloudFeAddress + "1";
                    localfe.Bind(localFeAddress);
                    localBeAddress = cloudFeAddress + "2";
                    localbe.Bind(localBeAddress);

                    Console.WriteLine("Press Enter when all brokers are started: ");
                    Console.ReadKey();

                    var workers = new List<Thread>();
                    for (int workerNumber = 0; workerNumber < numberOfWorkers; workerNumber++)
                    {
                        workers.Add(new Thread(WorkerTask));
                        workers[workerNumber].Start();
                    }

                    var clients = new List<Thread>();
                    for (int clientNumber = 0; clientNumber < numberOfClients; clientNumber++)
                    {
                        clients.Add(new Thread(ClientTask));
                        clients[clientNumber].Start();
                    }

                    var workerQueue = new Queue<byte[]>();

                    var localfeReady = false;
                    var cloudfeReady = false;

                    var backends = new PollItem[2];
                    backends[0] = localbe.CreatePollItem(IOMultiPlex.POLLIN);
                    backends[0].PollInHandler += (socket, revents) =>
                                                     {
                                                         var zmsg = new ZMessage(socket);

                                                         //  Use worker address for LRU routing
                                                         workerQueue.Enqueue(zmsg.Unwrap());

                                                         if (zmsg.BodyToString() != "READY")
                                                         {
                                                             SendReply(zmsg, cloudfe, localfe);
                                                         }
                                                     };

                    backends[1] = cloudbe.CreatePollItem(IOMultiPlex.POLLIN);
                    backends[1].PollInHandler += (socket, revents) =>
                    {
                        var zmsg = new ZMessage(socket);
                        //  We don't use peer broker address for anything
                        zmsg.Unwrap();

                        SendReply(zmsg, cloudfe, localfe);
                    };

                    var frontends = new PollItem[2];
                    frontends[0] = cloudfe.CreatePollItem(IOMultiPlex.POLLIN);
                    frontends[0].PollInHandler += (socket, revents) =>
                                                    {
                                                        cloudfeReady = true;
                                                    };

                    frontends[1] = localfe.CreatePollItem(IOMultiPlex.POLLIN);
                    frontends[1].PollInHandler += (socket, revents) =>
                                                     {
                                                         localfeReady = true;
                                                     };


                    while (true)
                    {
                        var timeout = (workerQueue.Count > 0 ? 1000000 : -1);
                        var rc = Context.Poller(backends, timeout);

                        if (rc == -1)
                            break; // Interrupted

                        while (workerQueue.Count > 0)
                        {
                            Context.Poller(frontends, 0);
                            bool reRoutable;

                            ZMessage msg;

                            if (cloudfeReady)
                            {
                                cloudfeReady = false;
                                msg = new ZMessage(cloudfe);
                                reRoutable = false;
                            }
                            else if (localfeReady)
                            {
                                localfeReady = false;
                                msg = new ZMessage(localfe);
                                reRoutable = true;
                            }
                            else
                            {
                                break;
                            }

                            //if (reRoutable && workerQueue.Count > 0 && randomizer.Next(3) == 0)
                            if (reRoutable && peers.Count > 0 && randomizer.Next(4) == 0)
                            {
                                var randomPeer = randomizer.Next(1, args.Length - 1);
                                var endpoint = "tcp://127.0.0.1:" + args[randomPeer];
                                msg.Wrap(Encoding.Unicode.GetBytes(endpoint), new byte[0]);
                                msg.Send(cloudbe);
                            }
                            else
                            {
                                msg.Wrap(workerQueue.Dequeue(), new byte[0]);
                                msg.Send(localbe);
                            }
                        }
                    }
                }
            }
        }

        private static void SendReply(ZMessage msg, Socket cloudfe, Socket localfe)
        {
            var address = Encoding.Unicode.GetString(msg.Address);
            //  Route reply to cloud if it's addressed to a broker

            if (peers.Count(peerAddress => peerAddress == address) == 1)
            {
                Console.WriteLine("Sending to cloud frontend");
                msg.Send(cloudfe);
            }
            else
            {
                Console.WriteLine("Sending to local frontend");
                msg.Send(localfe);
            }
        }

        private static void WorkerTask()
        {
            using (var ctx = new Context(1))
            {
                using (var worker = ctx.Socket(SocketType.REQ))
                {
                    ZHelpers.SetID(worker, Encoding.Unicode);
                    worker.Connect(localBeAddress);

                    var msg = new ZMessage("READY");
                    msg.Send(worker);

                    while (true)
                    {
                        var recvMsg = new ZMessage(worker);
                        Console.WriteLine("Worker: {0}", recvMsg.BodyToString());

                        Thread.Sleep(1000);

                        recvMsg.StringToBody("OK");
                        recvMsg.Send(worker);
                        //var okmsg = new ZMessage("OK");
                        //okmsg.Send(worker);
                    }
                }
            }
        }

        private static void ClientTask()
        {
            using (var ctx = new Context(1))
            {
                using (var client = ctx.Socket(SocketType.REQ))
                {
                    ZHelpers.SetID(client, Encoding.Unicode);
                    client.Connect(localFeAddress);

                    while (true)
                    {
                        client.Send("HELLO", Encoding.Unicode);
                        string reply = client.Recv(Encoding.Unicode);

                        if (string.IsNullOrEmpty(reply))
                        {
                            break;
                        }

                        Console.WriteLine("Client: {0}", reply);

                        Thread.Sleep(1000);
                    }
                }
            }
        }
    }
}
