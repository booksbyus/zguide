using System;
using System.Collections.Generic;
using System.Threading;
using System.Text;

/**
 *  Author: Randy Dryburgh
 *  Email: me@rwd.im
 *  License: This example code licensed under the MIT/X11 license.
 */

namespace Examples {
    class hwserver {
        static void Main(string[] args) {
            // allocate a buffer
            byte[] zmq_buffer = new byte[1024];

            //  Prepare our context and socket
            ZMQ.Context context = new ZMQ.Context(1);
            ZMQ.Socket  socket = context.Socket(ZMQ.REP);
            socket.Bind("tcp://*:5555");

            while (true) {
                try {
                    //  Wait for next request from client
                    socket.Recv(out zmq_buffer);
                    string request = Encoding.ASCII.GetString(zmq_buffer);

                    // log that we got one
                    Console.WriteLine("Received request: [%s]", request);

                    //  Do some 'work'
                    Thread.Sleep(1);

                    //  Send reply back to client
                    socket.Send(Encoding.ASCII.GetBytes("World".ToCharArray()));

                } catch (ZMQ.Exception z) {
                    // report the exception
                    Console.WriteLine("ZMQ Exception occurred : {0}", z.Message);
                }
            }
        }
    }
}