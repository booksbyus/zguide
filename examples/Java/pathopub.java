package guide;

import java.util.Random;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;

//  Pathological publisher
//  Sends out 1,000 topics and then one random update per second
public class pathopub
{
    public static void main(String[] args) throws Exception
    {
        try (ZContext context = new ZContext()) {
            Socket publisher = context.createSocket(SocketType.PUB);
            if (args.length == 1)
                publisher.connect(args[0]);
            else publisher.bind("tcp://*:5556");

            //  Ensure subscriber connection has time to complete
            Thread.sleep(1000);

            //  Send out all 1,000 topic messages
            int topicNbr;
            for (topicNbr = 0; topicNbr < 1000; topicNbr++) {
                publisher.send(String.format("%03d", topicNbr), ZMQ.SNDMORE);
                publisher.send("Save Roger");
            }
            //  Send one random update per second
            Random rand = new Random(System.currentTimeMillis());
            while (!Thread.currentThread().isInterrupted()) {
                Thread.sleep(1000);
                publisher.send(
                    String.format("%03d", rand.nextInt(1000)), ZMQ.SNDMORE
                );
                publisher.send("Off with his head!");
            }
        }
    }
}
