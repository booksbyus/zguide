package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * Pubsub envelope subscriber
 */

public class psenvsub
{

    public static void main(String[] args)
    {
        // Prepare our context and subscriber
        try (ZContext context = new ZContext()) {
            Socket subscriber = context.createSocket(SocketType.SUB);
            subscriber.connect("tcp://localhost:5563");
            subscriber.subscribe("B".getBytes(ZMQ.CHARSET));

            while (!Thread.currentThread().isInterrupted()) {
                // Read envelope with address
                String address = subscriber.recvStr();
                // Read message contents
                String contents = subscriber.recvStr();
                System.out.println(address + " : " + contents);
            }
        }
    }
}
