import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Clone client Model Five
 *
 */
public class Clonecli5
{
    //  This client is identical to Clonecli3 except for where we
    //  handles subtrees.
    private final static String SUBTREE  = "/client/";


	public void run() {
		ZContext ctx = new ZContext();
		Socket snapshot = ctx.createSocket(ZMQ.DEALER);
		snapshot.connect("tcp://localhost:5556");

		Socket subscriber = ctx.createSocket(ZMQ.SUB);
        subscriber.connect("tcp://localhost:5557");
        subscriber.subscribe(SUBTREE.getBytes());

		Socket publisher = ctx.createSocket(ZMQ.PUSH);
		publisher.connect("tcp://localhost:5558");

        Map<String, Kvmsg> kvMap = new HashMap<String, Kvmsg>();

        // get state snapshot
		snapshot.sendMore("ICANHAZ?");
        snapshot.send(SUBTREE);
        long sequence = 0;

        while (true) {
            Kvmsg kvMsg = Kvmsg.recv(snapshot);
            if (kvMsg == null)
                break;      //  Interrupted

			sequence = kvMsg.getSequence();
			if ("KTHXBAI".equalsIgnoreCase(kvMsg.getKey())) {
				System.out.println("Received snapshot = " + kvMsg.getSequence());
                kvMsg.destroy();
				break; // done
			}

            kvMsg.dump();
			System.out.println("receiving " + kvMsg.getSequence());
            kvMsg.store(kvMap);
		}

		Poller poller = new Poller(1);
		poller.register(subscriber);

		Random random = new Random();

		// now apply pending updates, discard out-of-getSequence messages
		long alarm = System.currentTimeMillis() + 5000;
		while (true) {
			int rc = poller.poll(Math.max(0, alarm - System.currentTimeMillis()));
            if (rc == -1)
                break;              //  Context has been shut down

			if (poller.pollin(0)) {
                Kvmsg kvMsg = Kvmsg.recv(subscriber);
                if (kvMsg == null)
                    break;      //  Interrupted

                if (kvMsg.getSequence() > sequence) {
                    sequence = kvMsg.getSequence();
                    System.out.println("receiving " + sequence);
                    kvMsg.store(kvMap);
                } else
                    kvMsg.destroy();
			}

			if (System.currentTimeMillis() >= alarm) {
				Kvmsg kvMsg = new Kvmsg(0);
                kvMsg.fmtKey("%s%d", SUBTREE, random.nextInt(10000));
                kvMsg.fmtBody("%d", random.nextInt(1000000));
                kvMsg.setProp("ttl", "%d", random.nextInt(30));
                kvMsg.send(publisher);
                kvMsg.destroy();

				alarm = System.currentTimeMillis() + 1000;
			}
		}
        ctx.destroy();
	}

	public static void main(String[] args) {
		new Clonecli5().run();
	}
}
