package guide;

import org.zeromq.SocketType;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZContext;

/**
* Synchronized publisher.
*/
public class syncpub
{
    /**
     * We wait for 10 subscribers
     */
    protected static int SUBSCRIBERS_EXPECTED = 10;

    public static void main(String[] args)
    {
        try (ZContext context = new ZContext()) {
            //  Socket to talk to clients
            Socket publisher = context.createSocket(SocketType.PUB);
            publisher.setLinger(5000);
            // In 0MQ 3.x pub socket could drop messages if sub can follow the
            // generation of pub messages
            publisher.setSndHWM(0);
            publisher.bind("tcp://*:5561");

            //  Socket to receive signals
            Socket syncservice = context.createSocket(SocketType.REP);
            syncservice.bind("tcp://*:5562");

            System.out.println("Waiting for subscribers");
            //  Get synchronization from subscribers
            int subscribers = 0;
            while (subscribers < SUBSCRIBERS_EXPECTED) {
                //  - wait for synchronization request
                syncservice.recv(0);

                //  - send synchronization reply
                syncservice.send("", 0);
                subscribers++;
            }
            //  Now broadcast exactly 1M updates followed by END
            System.out.println("Broadcasting messages");

            int update_nbr;
            for (update_nbr = 0; update_nbr < 1000000; update_nbr++) {
                publisher.send("Rhubarb", 0);
            }

            publisher.send("END", 0);
        }
    }
}
