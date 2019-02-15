package guide;

import org.zeromq.*;
import org.zeromq.ZMQ.Socket;

//  Freelance server - Model 3
//  Uses an ROUTER/ROUTER socket but just one thread
public class flserver3
{
    public static void main(String[] args)
    {
        boolean verbose = (args.length > 0 && args[0].equals("-v"));

        try (ZContext ctx = new ZContext()) {
            //  Prepare server socket with predictable identity
            String bindEndpoint = "tcp://*:5555";
            String connectEndpoint = "tcp://localhost:5555";
            Socket server = ctx.createSocket(SocketType.ROUTER);
            server.setIdentity(connectEndpoint.getBytes(ZMQ.CHARSET));
            server.bind(bindEndpoint);
            System.out.printf("I: service is ready at %s\n", bindEndpoint);

            while (!Thread.currentThread().isInterrupted()) {
                ZMsg request = ZMsg.recvMsg(server);
                if (verbose && request != null)
                    request.dump(System.out);

                if (request == null)
                    break; //  Interrupted

                //  Frame 0: identity of client
                //  Frame 1: PING, or client control frame
                //  Frame 2: request body
                ZFrame identity = request.pop();
                ZFrame control = request.pop();
                ZMsg reply = new ZMsg();
                if (control.equals(new ZFrame("PING")))
                    reply.add("PONG");
                else {
                    reply.add(control);
                    reply.add("OK");
                }
                request.destroy();
                reply.push(identity);
                if (verbose && reply != null)
                    reply.dump(System.out);
                reply.send(server);
            }

            if (Thread.currentThread().isInterrupted())
                System.out.printf("W: interrupted\n");
        }
    }
}
