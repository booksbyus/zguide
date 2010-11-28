//
//  Request-reply client
//  Connects REQ socket to tcp://localhost:5559
//  Sends "Hello" to server, expects "World" back
//

//  Author:     Michael Compton
//  Email:      michael.compton@littleedge.co.uk

using System;
using System.Text;
using ZMQ;

namespace ZMQGuide {
    class Program {
        static void Main(string[] args) {
            // Prepare our context and sockets
            using (Context context = new Context(1)) {
                using (Socket socket = context.Socket(SocketType.REQ)) {
                    socket.Connect("tcp://localhost:5559");

                    // Do 10 requests, waiting each time for a response
                    for (int count = 0; count < 10; count++) {
                        socket.Send("Hello", Encoding.Unicode);
                        string message = socket.Recv(Encoding.Unicode);
                        Console.WriteLine("Received reply: " + message);
                    }
                }
            }
        }
    }
}
