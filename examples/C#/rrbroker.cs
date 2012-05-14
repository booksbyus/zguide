//
//  Simple request-reply broker
//

//  Author:     Michael Compton, Tomas Roos
//  Email:      michael.compton@littleedge.co.uk, ptomasroos@gmail.com

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            using (var context = new Context(1))
            {
                using (Socket frontend = context.Socket(SocketType.ROUTER), backend = context.Socket(SocketType.DEALER))
                {
                    frontend.Bind("tcp://*:5559");
                    backend.Bind("tcp://*:5560");

                    var pollItems = new PollItem[2];
                    pollItems[0] = frontend.CreatePollItem(IOMultiPlex.POLLIN);
                    pollItems[0].PollInHandler += (socket, revents) => FrontendPollInHandler(socket, backend);
                    pollItems[1] = backend.CreatePollItem(IOMultiPlex.POLLIN);
                    pollItems[1].PollInHandler += (socket, revents) => BackendPollInHandler(socket, frontend);

                    while (true)
                    {
                        context.Poll(pollItems, -1);
                    }
                }
            }
        }

        private static void FrontendPollInHandler(Socket frontend, Socket backend)
        {
            RelayMessage(frontend, backend);
        }

        private static void BackendPollInHandler(Socket backend, Socket frontend)
        {
            RelayMessage(backend, frontend);
        }

        private static void RelayMessage(Socket source, Socket destination)
        {
            bool hasMore = true;
            while (hasMore)
            {
                // side effect warning!
                // note! that this uses Recv mode that gets a byte[], the router c# implementation 
                // doesnt work if you get a string message instead of the byte[] i would prefer the solution thats commented.
                // but the router doesnt seem to be able to handle the response back to the client
                //string message = source.Recv(Encoding.Unicode);
                //hasMore = source.RcvMore;
                //destination.Send(message, Encoding.Unicode, hasMore ? SendRecvOpt.SNDMORE : SendRecvOpt.NONE);

                byte[] message = source.Recv();
                hasMore = source.RcvMore;
                destination.Send(message, hasMore ? SendRecvOpt.SNDMORE : SendRecvOpt.NONE);
            }
        }
    }
}
