//
//  Synchronized subscriber
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
            using (Context context = new Context(1)) {
                using (Socket subscriber = context.Socket(SocketType.SUB),
                    syncClient = context.Socket(SocketType.REQ)) {
                    
                    //  First, connect our subscriber socket
                    subscriber.Connect("tcp://localhost:5561");
                    subscriber.Subscribe("", Encoding.Unicode);

                    //  Second, synchronize with publisher
                    syncClient.Connect("tcp://localhost:5562");
                    
                    //  - send a synchronization request
                    syncClient.Send("", Encoding.Unicode);
                    //  - wait for synchronization reply
                    syncClient.Recv();
                    
                    //  Third, get our updates and report how many we got
                    int nbrUpdates = 0;
                    while (!subscriber.Recv(Encoding.Unicode).Equals("END")) {
                        nbrUpdates++;
                    }
                    Console.WriteLine("Received {0} updates.", nbrUpdates);
                }
            }
        }
    }
}
