//
//  Request-reply service
//  Connects REP socket to tcp://localhost:5560
//  Expects "Hello" from client, replies with "World"
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
                using (Socket socket = context.Socket(SocketType.REP)) {
                    socket.Connect("tcp://localhost:5560");
                    while (true) {
                        string message = socket.Recv(Encoding.Unicode);
                        Console.WriteLine("Received request: " + message);
                        socket.Send("World", Encoding.Unicode);
                    }
                }
            }
        }
    }
}
