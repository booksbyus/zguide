//
//  Durable subscriber
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
                using (Socket sync = context.Socket(SocketType.PUSH),
                    subscriber = context.Socket(SocketType.SUB)) {

                    //  Connect our subscriber socket
                    subscriber.StringToIdentity("Hello", Encoding.Unicode);
                    subscriber.Subscribe("", Encoding.Unicode);
                    subscriber.Connect("tcp://localhost:5565");
                    
                    //  Synchronize with publisher
                    sync.Connect("tcp://localhost:5564");
                    sync.Send("", Encoding.Unicode);
                    
                    //  Get updates, expect random Ctrl-C death
                    string message = "";
                    while (!message.Equals("END")) {
                        message = subscriber.Recv(Encoding.Unicode);
                        Console.WriteLine(message);
                    }
                }
            }
        }
    }
}
