//
//  Lazy Pirate client
//  Use zmq_poll to do a safe request-reply
//  To run, start lpserver and then randomly kill/restart it

//  Author:     Tomas Roos
//  Email:      ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using ZeroMQ;

namespace zguide.lpclient
{
    internal class Program
    {
        private static int requestTimeout = 2500;
        private static int requestRetries = 3;
        private static string serverEndpoint = "tcp://127.0.0.1:5555";

        private static int sequence = 0;
        private static bool expectReply = true;
        private static int retriesLeft = requestRetries;

        private static ZmqSocket CreateServerSocket(ZmqContext context)
        {
            Console.WriteLine("Connecting to server...");

            var client = context.CreateSocket(SocketType.REQ);
            client.Connect(serverEndpoint);
            client.Linger = TimeSpan.Zero;
            client.ReceiveReady += PollInHandler;

            return client;
        }

        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                var client = CreateServerSocket(context);

                while (retriesLeft > 0)
                {
                    sequence++;
                    Console.WriteLine("Sending ({0})", sequence);
                    client.Send(sequence.ToString(), Encoding.Unicode);
                    expectReply = true;

                    while (expectReply)
                    {
                        var poller = new Poller(new List<ZmqSocket> { client });
                        int count = poller.Poll(TimeSpan.FromMilliseconds(requestTimeout * 1000));

                        if (count == 0)
                        {
                            retriesLeft--;

                            if (retriesLeft == 0)
                            {
                                Console.WriteLine("Server seems to be offline, abandoning");
                                break;
                            }
                            else
                            {
                                Console.WriteLine("No response from server, retrying..");

                                client = null;
                                client = CreateServerSocket(context);
                                client.Send(sequence.ToString(), Encoding.Unicode);
                            }
                        }
                    }
                }
            }
        }

        private static void PollInHandler(object sender, SocketEventArgs e)
        {
            var reply = e.Socket.Receive(Encoding.Unicode);

            if (Int32.Parse(reply) == sequence)
            {
                Console.WriteLine("Server replied OK ({0})", reply);
                retriesLeft = requestRetries;
                expectReply = false;
            }
            else
            {
                Console.WriteLine("Malformed reply from server: {0}", reply);
            }

        }
    }
}