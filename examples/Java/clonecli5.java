package guide;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

/**
 * Clone client Model Five
 *
 */
public class clonecli5
{
    //  This client is identical to clonecli3 except for where we
    //  handles subtrees.
    private final static String SUBTREE = "/client/";

    public void run()
    {
        try (ZContext ctx = new ZContext()) {
            Socket snapshot = ctx.createSocket(SocketType.DEALER);
            snapshot.connect("tcp://localhost:5556");

            Socket subscriber = ctx.createSocket(SocketType.SUB);
            subscriber.connect("tcp://localhost:5557");
            subscriber.subscribe(SUBTREE.getBytes(ZMQ.CHARSET));

            Socket publisher = ctx.createSocket(SocketType.PUSH);
            publisher.connect("tcp://localhost:5558");

            Map<String, kvmsg> kvMap = new HashMap<String, kvmsg>();

            // get state snapshot
            snapshot.sendMore("ICANHAZ?");
            snapshot.send(SUBTREE);
            long sequence = 0;

            while (true) {
                kvmsg kvMsg = kvmsg.recv(snapshot);
                if (kvMsg == null)
                    break; //  Interrupted

                sequence = kvMsg.getSequence();
                if ("KTHXBAI".equalsIgnoreCase(kvMsg.getKey())) {
                    System.out.println(
                        "Received snapshot = " + kvMsg.getSequence()
                    );
                    kvMsg.destroy();
                    break; // done
                }

                System.out.println("receiving " + kvMsg.getSequence());
                kvMsg.store(kvMap);
            }

            Poller poller = ctx.createPoller(1);
            poller.register(subscriber);

            Random random = new Random();

            // now apply pending updates, discard out-of-getSequence messages
            long alarm = System.currentTimeMillis() + 5000;
            while (true) {
                int rc = poller.poll(
                    Math.max(0, alarm - System.currentTimeMillis())
                );
                if (rc == -1)
                    break; //  Context has been shut down

                if (poller.pollin(0)) {
                    kvmsg kvMsg = kvmsg.recv(subscriber);
                    if (kvMsg == null)
                        break; //  Interrupted

                    if (kvMsg.getSequence() > sequence) {
                        sequence = kvMsg.getSequence();
                        System.out.println("receiving " + sequence);
                        kvMsg.store(kvMap);
                    }
                    else kvMsg.destroy();
                }

                if (System.currentTimeMillis() >= alarm) {
                    kvmsg kvMsg = new kvmsg(0);
                    kvMsg.fmtKey("%s%d", SUBTREE, random.nextInt(10000));
                    kvMsg.fmtBody("%d", random.nextInt(1000000));
                    kvMsg.setProp("ttl", "%d", random.nextInt(30));
                    kvMsg.send(publisher);
                    kvMsg.destroy();

                    alarm = System.currentTimeMillis() + 1000;
                }
            }
        }
    }

    public static void main(String[] args)
    {
        new clonecli5().run();
    }
}
