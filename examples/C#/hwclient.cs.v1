using System;
using System.Collections.Generic;
using System.Threading;
using System.Text;

/**
 *  Author: John Unwin
 *  Email: john@kaitrade.com
 *  License: This example code licensed under the MIT/X11 license.
 */

namespace Examples
{
    class hwclient
    {
        static void Main(string[] args)
        {
            // allocate a buffer
            byte[] zmq_buffer = new byte[1024];

            //  Prepare our context and socket
            ZMQ.Context context = new ZMQ.Context(1);
            ZMQ.Socket socket = context.Socket(ZMQ.REQ);
            socket.Connect("tcp://localhost:5555");

            string request = "";
            for (long requestNum = 0; requestNum != 10; requestNum++)
            {
                socket.Send(Encoding.ASCII.GetBytes("Hello".ToCharArray()));
                //  Wait for next request from client
                socket.Recv(out zmq_buffer);
                request = Encoding.ASCII.GetString(zmq_buffer);
           }
        }
    }
}