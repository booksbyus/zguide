import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;

import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
/**
 * Clone client Model Three
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class Clonecli3 {
	private static Map<String, Kvsimple> kvMap = new HashMap<String, Kvsimple>();

	public void run() {
		ZContext ctx = new ZContext();
		Socket snapshot = ctx.createSocket(ZMQ.DEALER);
		snapshot.connect("tcp://localhost:5556");

		Socket subscriber = ctx.createSocket(ZMQ.SUB);
		subscriber.connect("tcp://localhost:5557");
		subscriber.subscribe("".getBytes());

		Socket push = ctx.createSocket(ZMQ.PUSH);
		push.connect("tcp://localhost:5558");

		// get state snapshot
        long sequence = 0;
        snapshot.send("ICANHAZ?".getBytes(), 0);
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
			Clonecli3.kvMap.put(kvMsg.getKey(), kvMsg);
		}

		Poller poller = new ZMQ.Poller(1);
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
                    Clonecli3.kvMap.put(kvMsg.getKey(), kvMsg);
                }
			}

			if (System.currentTimeMillis() >= alarm) {
				int key = random.nextInt(10000);
				int body = random.nextInt(1000000);

				ByteBuffer b = ByteBuffer.allocate(4);
				b.asIntBuffer().put(body);

				Kvsimple kvUpdateMsg = new Kvsimple(key + "", 0, b.array());
				kvUpdateMsg.send(push);
				alarm = System.currentTimeMillis() + 1000;
			}
		}
        ctx.destroy();
	}

	public static void main(String[] args) {
		new Clonecli3().run();
	}
}
