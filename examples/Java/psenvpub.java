import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Pubsub envelope publisher
 */

public class psenvpub {

    public static void main (String[] args) throws Exception {
        // Prepare our context and publisher
        Context context = ZMQ.context(1);
        Socket publisher = context.socket(ZMQ.PUB);

        publisher.bind("tcp://*:5563");
        while (!Thread.currentThread ().isInterrupted ()) {
            // Write two messages, each with an envelope and content
            publisher.sendMore ("A");
            publisher.send ("We don't want to see this");
            publisher.sendMore ("B");
            publisher.send("We would like to see this");
        }
        publisher.close ();
        context.term ();
    }
}
