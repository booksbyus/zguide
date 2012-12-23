//
//  Simple request-reply broker
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZeroMQ;
using ZeroMQ.Interop;

namespace zguide.rrbroker
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = ZmqContext.Create())
            {
                using (ZmqSocket frontend = context.CreateSocket(SocketType.ROUTER), backend = context.CreateSocket(SocketType.DEALER))
                {
                    frontend.Bind("tcp://*:5559");
                    backend.Bind("tcp://*:5560");

                    var pollItems = new PollItem[2];
                    pollItems[0] = frontend.CreatePollItem(Poller.POLLIN);
                    pollItems[0].PollInHandler += (ZmqSocket, revents) => FrontendPollInHandler(ZmqSocket, backend);
                    pollItems[1] = backend.CreatePollItem(Poller.POLLIN);
                    pollItems[1].PollInHandler += (ZmqSocket, revents) => BackendPollInHandler(ZmqSocket, frontend);

                    while (true)
                    {
                        context.Poll(pollItems, -1);
                    }
                }
            }
        }

        private static void FrontendPollInHandler(ZmqSocket frontend, ZmqSocket backend)
        {
            RelayMessage(frontend, backend);
        }

        private static void BackendPollInHandler(ZmqSocket backend, ZmqSocket frontend)
        {
            RelayMessage(backend, frontend);
        }

        private static void RelayMessage(ZmqSocket source, ZmqSocket destination)
        {
            bool hasMore = true;
            while (hasMore)
            {
                // side effect warning!
                // note! that this uses Receive mode that gets a byte[], the router c# implementation 
                // doesnt work if you get a string message instead of the byte[] i would prefer the solution thats commented.
                // but the router doesnt seem to be able to handle the response back to the client
                //string message = source.Receive(Encoding.Unicode);
                //hasMore = source.RcvMore;
                //destination.Send(message, Encoding.Unicode, hasMore ? SendRecvOpt.SNDMORE : SendRecvOpt.NONE);

                byte[] message = source.Receive();
                hasMore = source.ReceiveMore;
                destination.Send(message, hasMore ? SocketFlags.SendMore : SocketFlags.None);
            }
        }
    }
}
