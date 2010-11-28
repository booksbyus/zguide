//
//  Reading from multiple sockets
//  This version uses a simple recv loop
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
            //  Prepare our context and socket
            using (Context context = new Context(1)) {
                //  Connect to task ventilator and weather server
                using (Socket receiver = context.Socket(SocketType.PULL),
                    subscriber = context.Socket(SocketType.SUB)) {
                    receiver.Connect("tcp://localhost:5557");
                    subscriber.Connect("tcp://localhost:5556");
                    subscriber.Subscribe("10001 ", Encoding.Unicode);

                    //  Process messages from both sockets
                    //  We prioritize traffic from the task ventilator
                    while (true) {
                        //  Process any waiting tasks
                        while (true) {
                            byte[] msg = receiver.Recv(SendRecvOpt.NOBLOCK);
                            if (msg != null) {
                                Console.WriteLine("Process Task");
                            } else {
                                break;
                            }
                        }
                        //  Process any waiting weather updates
                        while (true) {
                            byte[] msg = subscriber.Recv(SendRecvOpt.NOBLOCK);
                            if (msg != null) {
                                Console.WriteLine("Process Weather");
                            } else {
                                break;
                            }
                        }
                        Thread.Sleep(1000);
                    }
                }
            }
        }
    }
}
