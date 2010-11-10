//
//  Pubsub envelope publisher
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            //  Prepare our context and publisher
            using (Context context = new Context(1)) {
                using (Socket publisher = context.Socket(SocketType.PUB)) {
                    publisher.Bind("tcp://*:5563");
                    while (true) {
                        //  Write two messages, each with an envelope and content
                        publisher.SendMore("A", Encoding.Unicode);
                        publisher.Send("We don't want to see this.", Encoding.Unicode);
                        publisher.SendMore("B", Encoding.Unicode);
                        publisher.Send("We would like to see this.", Encoding.Unicode);
                        Thread.Sleep(1000);
                    }
                }
            }
        }
    }
}
