package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
* Simple message queuing broker
* Same as request-reply broker but using QUEUE device.
*/
public class msgqueue
{

    public static void main(String[] args)
    {
        //  Prepare our context and sockets
        try (ZContext context = new ZContext()) {
            //  Socket facing clients
            Socket frontend = context.createSocket(SocketType.ROUTER);
            frontend.bind("tcp://*:5559");

            //  Socket facing services
            Socket backend = context.createSocket(SocketType.DEALER);
            backend.bind("tcp://*:5560");

            //  Start the proxy
            ZMQ.proxy(frontend, backend, null);
        }
    }
}
