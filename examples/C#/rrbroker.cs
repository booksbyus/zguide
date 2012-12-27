//
//  Simple request-reply broker
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Collections.Generic;
using System.Linq;
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

                    frontend.ReceiveReady += (sender, e) => FrontendPollInHandler(e.Socket, backend);
                    backend.ReceiveReady += (sender, e) => BackendPollInHandler(e.Socket, backend);

                    var poller = new Poller(new List<ZmqSocket> { frontend, backend });

                    while (true)
                    {
                        poller.Poll();
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

                byte[] message = new byte[0];
                source.Receive(message);
                hasMore = source.ReceiveMore;
                destination.Send(message, message.Length, hasMore ? SocketFlags.SendMore : SocketFlags.None);
            }
        }
    }
}
