//
//  Weather proxy device
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            using (Context context = new Context(1)) {
                using (Socket frontend = context.Socket(SocketType.SUB),
                    backend = context.Socket(SocketType.PUB)) {
                    //  This is where the weather server sits
                    frontend.Connect("tcp://192.168.55.210:5556");
                    //  Subscribe on everything
                    frontend.Subscribe("", Encoding.Unicode);

                    //  This is our public endpoint for subscribers
                    backend.Bind("tcp://10.1.1.0:8100");

                    //  Shunt messages out to our own subscribers
                    bool isProcessing = true;
                    while (isProcessing) {
                        byte[] message = frontend.Recv();
                        backend.Send(message, SendRecvOpt.SNDMORE);
                        isProcessing = frontend.RcvMore;
                    }
                }
            }
        }
    }
}
