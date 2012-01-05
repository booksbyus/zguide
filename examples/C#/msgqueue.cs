//
//  Simple message queuing broker
//  Same as request-reply broker but using QUEUE device
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
                using (Socket frontend = context.Socket(SocketType.ROUTER),
                backend = context.Socket(SocketType.DEALER)) {
                    //  Socket facing clients
                    frontend.Bind("tcp://*:5559");
                    //  Socket facing services
                    backend.Bind("tcp://*:5560");

                    //  Start built-in device
                    Socket.Device.Queue(frontend, backend);
                }
            }
        }
    }
}
