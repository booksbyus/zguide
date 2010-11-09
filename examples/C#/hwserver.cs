//
//  Hello World server
//  Binds REP socket to tcp://*:5555
//  Expects "Hello" from client, replies with "World"
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
            // ZMQ Context
            using (Context context = new Context(1)) {
                // Socket to talk to clients
                using (Socket socket = context.Socket(SocketType.REP)) {
                    socket.Bind("tcp://*:5555");
                    
                    while (true) {
                        // Wait for next request from client
                        string message = socket.Recv(Encoding.Unicode);
                        Console.WriteLine("Received request: {0}", message);

                        // Do Some 'work'
                        Thread.Sleep(1000);

                        // Send reply back to client
                        socket.Send("World", Encoding.Unicode);
                    }
                }
            }
        }
    }
}
