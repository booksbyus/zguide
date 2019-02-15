package guide;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMsg;

//  Freelance server - Model 1
//  Trivial echo service
public class flserver1
{
    public static void main(String[] args)
    {
        if (args.length < 1) {
            System.out.printf("I: syntax: flserver1 <endpoint>\n");
            System.exit(0);
        }

        try (ZContext ctx = new ZContext()) {
            Socket server = ctx.createSocket(SocketType.REP);
            server.bind(args[0]);

            System.out.printf("I: echo service is ready at %s\n", args[0]);

            while (true) {
                ZMsg msg = ZMsg.recvMsg(server);
                if (msg == null)
                    break; //  Interrupted
                msg.send(server);
            }

            if (Thread.currentThread().isInterrupted())
                System.out.printf("W: interrupted\n");
        }
    }
}
