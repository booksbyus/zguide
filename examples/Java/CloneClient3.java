import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
/**
 * Clone client Model Three
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class CloneClient3 {
	private static Map<String, KVSimple> kvMap = new HashMap<String, KVSimple>();
	private static AtomicLong sequence = new AtomicLong();

	public void run() {
		Context ctx = ZMQ.context(1);
		Socket snapshot = ctx.socket(ZMQ.XREQ);
		snapshot.setLinger(0);
		snapshot.connect("tcp://localhost:5556");

		Socket subscriber = ctx.socket(ZMQ.SUB);
		subscriber.setLinger(0);
		subscriber.connect("tcp://localhost:5557");
		subscriber.subscribe("".getBytes());

		Socket push = ctx.socket(ZMQ.PUSH);
		push.setLinger(0);
		push.connect("tcp://localhost:5558");

		// get state snapshot
		snapshot.send("ICANHAZ?".getBytes(), 0);
		KVSimple kvMsg = null;
		while (true) {
			kvMsg = KVSimple.recv(snapshot);
			sequence.set(kvMsg.getSequence());
			if ("KTHXBAI".equalsIgnoreCase(kvMsg.getKey())) {
				System.out.println("Received snapshot = " + kvMsg.getSequence());
				break; // done
			}

			System.out.println("receiving " + kvMsg.getSequence());
			CloneClient3.kvMap.put(kvMsg.getKey(), kvMsg);
		}

		Poller poller = ctx.poller();
		poller.register(subscriber);

		Random random = new Random();

		// now apply pending updates, discard out-of-sequence messages
		long alarm = System.currentTimeMillis() + 5000;
		while (true) {
			poller.poll(Math.max(0, alarm - System.currentTimeMillis()));

			if (poller.pollin(0)) {
				kvMsg = KVSimple.recv(subscriber);
				if (kvMsg != null) {
					if (kvMsg.getSequence() > sequence.get()) {
						sequence.set(kvMsg.getSequence());
						System.out.println("receiving " + sequence);
						CloneClient3.kvMap.put(kvMsg.getKey(), kvMsg);
					}
				}
			}

			if (System.currentTimeMillis() >= alarm) {
				System.out.println("Alarm time");
				int key = random.nextInt(10000);
				int body = random.nextInt(1000000);

				ByteBuffer b = ByteBuffer.allocate(4);
				b.asIntBuffer().put(body);

				KVSimple kvUpdateMsg = new KVSimple(key + "", 0, b.array());
				kvUpdateMsg.send(push);
				alarm = System.currentTimeMillis() + 5000;
			}
		}
	}

	public static void main(String[] args) {
		new CloneClient3().run();
	}
}
