//
//  Pubsub envelope subscriber
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
            //  Prepare our context and subscriber  
            using (Context context = new Context(1)) {
                using (Socket subscriber = context.Socket(SocketType.SUB)) {
                    subscriber.Connect("tcp://localhost:5563");
                    subscriber.Subscribe("B", Encoding.Unicode);

                    while (true) {
                        //  Read envelope with address
                        string address = subscriber.Recv(Encoding.Unicode);
                        //  Read message contents
                        string contents = subscriber.Recv(Encoding.Unicode);
                        Console.WriteLine("{0} : {1}", address, contents);
                    }
                }
            }
        }
    }
}
