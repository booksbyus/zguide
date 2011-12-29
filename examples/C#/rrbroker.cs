//
//  Simple request-reply broker
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide {
    public class RRBroker {
        private Context context;
        private Socket backend;
        private Socket frontend;

        public RRBroker() {
            //  Prepare our context and sockets
            context = new Context(1);
            frontend = context.Socket(SocketType.ROUTER);
            backend = context.Socket(SocketType.DEALER);
            frontend.Bind("tcp://*:5559");
            backend.Bind("tcp://*:5560");
        }

        public void Broker() {
            //  Initialize poll set
            PollItem[] pollItems = new PollItem[2];
            pollItems[0] = frontend.CreatePollItem(IOMultiPlex.POLLIN);
            pollItems[0].PollInHandler += new PollHandler(FrontendPollInHandler);
            pollItems[1] = backend.CreatePollItem(IOMultiPlex.POLLIN);
            pollItems[1].PollInHandler += new PollHandler(BackendPollInHandler);
            //  Switch messages between sockets
            while (true) {
                context.Poll(pollItems, -1);
            }
        }

        private void FrontendPollInHandler(Socket socket, IOMultiPlex revents) {
            //  Process all parts of the message
            bool isProcessing = true;
            while (isProcessing) {
                byte[] message = socket.Recv();
                backend.Send(message, socket.RcvMore ? SendRecvOpt.SNDMORE : 0);
                isProcessing = socket.RcvMore;
            }
        }

        private void BackendPollInHandler(Socket socket, IOMultiPlex revents) {
            //  Process all parts of the message
            bool isProcessing = true;
            while (isProcessing) {
                byte[] message = socket.Recv();
                frontend.Send(message, socket.RcvMore ? SendRecvOpt.SNDMORE : 0);
                isProcessing = socket.RcvMore;
            }
        }
    }

    class Program {
        static void Main(string[] args) {
            RRBroker broker = new RRBroker();
            broker.Broker();
        }
    }
}
