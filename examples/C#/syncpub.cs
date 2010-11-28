//
//  Synchronized publisher
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using System.Threading;
using ZMQ;

namespace ZMQGuide {
    class Program {
        const int NUM_OF_SUBSCRIBERS = 10;

        static void Main(string[] args) {
            using (Context context = new Context(1)) {
                using (Socket publisher = context.Socket(SocketType.PUB),
                    syncService = context.Socket(SocketType.REP)) {
                    //  Socket to talk to clients
                    publisher.Bind("tcp://*:5561");
                    //  Socket to receive signals
                    syncService.Bind("tcp://*:5562");

                    //  Get synchronization from subscribers
                    for (int count = 0; count < NUM_OF_SUBSCRIBERS; count++) {
                        syncService.Recv();
                        syncService.Send("", Encoding.Unicode);
                    }
                    //  Now broadcast exactly 1M updates followed by END
                    for (int nbrOfUpdates = 0; nbrOfUpdates < 1000000; nbrOfUpdates++) {
                        publisher.Send("Rhubard", Encoding.Unicode);
                    }
                    publisher.Send("END", Encoding.Unicode);
                    Thread.Sleep(1000);  //  Give 0MQ/2.0.x time to flush output
                }
            }
        }
    }
}
