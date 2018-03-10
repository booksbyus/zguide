import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Clone client Model Four
 *
 */
public class Clonecli4
{
    //  This client is identical to Clonecli3 except for where we
    //  handles subtrees.
    private final static String SUBTREE  = "/client/";

	private static Map<String, Kvsimple> kvMap = new HashMap<String, Kvsimple>();

	public void run() {
		ZContext ctx = new ZContext();
		Socket snapshot = ctx.createSocket(ZMQ.DEALER);
		snapshot.connect("tcp://localhost:5556");

		Socket subscriber = ctx.createSocket(ZMQ.SUB);
        subscriber.connect("tcp://localhost:5557");
        subscriber.subscribe(SUBTREE.getBytes());

		Socket push = ctx.createSocket(ZMQ.PUSH);
		push.connect("tcp://localhost:5558");

		// get state snapshot
		snapshot.sendMore("ICANHAZ?");
        snapshot.send(SUBTREE);
        long sequence = 0;

        while (true) {
            Kvsimple kvMsg = Kvsimple.recv(snapshot);
            if (kvMsg == null)
                break;      //  Interrupted

			sequence = kvMsg.getSequence();
			if ("KTHXBAI".equalsIgnoreCase(kvMsg.getKey())) {
				System.out.println("Received snapshot = " + kvMsg.getSequence());
				break; // done
			}

			System.out.println("receiving " + kvMsg.getSequence());
			Clonecli4.kvMap.put(kvMsg.getKey(), kvMsg);
		}

		Poller poller = new Poller(1);
		poller.register(subscriber);

		Random random = new Random();

		// now apply pending updates, discard out-of-sequence messages
		long alarm = System.currentTimeMillis() + 5000;
		while (true) {
			int rc = poller.poll(Math.max(0, alarm - System.currentTimeMillis()));
            if (rc == -1)
                break;              //  Context has been shut down

			if (poller.pollin(0)) {
                Kvsimple kvMsg = Kvsimple.recv(subscriber);
                if (kvMsg == null)
                    break;      //  Interrupted
                if (kvMsg.getSequence() > sequence) {
                    sequence = kvMsg.getSequence();
                    System.out.println("receiving " + sequence);
                    Clonecli4.kvMap.put(kvMsg.getKey(), kvMsg);
                }
			}

			if (System.currentTimeMillis() >= alarm) {
				String key = String.format("%s%d", SUBTREE, random.nextInt(10000));
				int body = random.nextInt(1000000);

				ByteBuffer b = ByteBuffer.allocate(4);
				b.asIntBuffer().put(body);

				Kvsimple kvUpdateMsg = new Kvsimple(key, 0, b.array());
				kvUpdateMsg.send(push);
				alarm = System.currentTimeMillis() + 1000;
			}
		}
        ctx.destroy();
	}

	public static void main(String[] args) {
		new Clonecli4().run();
	}
}
