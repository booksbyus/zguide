package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
 * Pubsub envelope publisher
 */

public class psenvpub
{

    public static void main(String[] args) throws Exception
    {
        // Prepare our context and publisher
        try (ZContext context = new ZContext()) {
            Socket publisher = context.createSocket(SocketType.PUB);
            publisher.bind("tcp://*:5563");

            while (!Thread.currentThread().isInterrupted()) {
                // Write two messages, each with an envelope and content
                publisher.sendMore("A");
                publisher.send("We don't want to see this");
                publisher.sendMore("B");
                publisher.send("We would like to see this");
            }
        }
    }
}
