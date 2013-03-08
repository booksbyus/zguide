import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Clone client Model Two
 * 
 * @author Danish Shrestha <dshrestha06@gmail.com>
 * 
 */
public class CloneClient2 {
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
			CloneClient2.kvMap.put(kvMsg.getKey(), kvMsg);
		}

		// now apply pending updates, discard out-of-sequence messages
		while (true) {
			kvMsg = null;
			kvMsg = KVSimple.recv(subscriber);

			if (kvMsg != null) {
				if (kvMsg.getSequence() > sequence.get()) {
					sequence.set(kvMsg.getSequence());
					System.out.println("receiving " + sequence);
					CloneClient2.kvMap.put(kvMsg.getKey(), kvMsg);
				}
			}
		}
	}

	public static void main(String[] args) {
		new CloneClient2().run();
	}
}
