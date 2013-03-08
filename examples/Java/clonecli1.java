import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;
/**
 * Clone client model 1
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class clonecli1 {
	private static Map<String, kvsimple> kvMap = new HashMap<String, kvsimple>();
	private static AtomicLong sequence = new AtomicLong();

	public void run() {
		Context ctx = ZMQ.context(1);
		Socket subscriber = ctx.socket(ZMQ.SUB);
		subscriber.setLinger(0);
		subscriber.connect("tcp://localhost:5556");
		subscriber.subscribe("".getBytes());

		while (true) {
			kvsimple kvMsg = null;
			try {
				kvMsg = kvsimple.recv(subscriber);
			} catch (Throwable t) {
				// cant let it die
			}

			if (kvMsg != null) {
				clonecli1.kvMap.put(kvMsg.getKey(), kvMsg);
				System.out.println("receiving " + kvMsg);
				sequence.incrementAndGet();
			}
		}
	}

	public static void main(String[] args) {
		new clonecli1().run();
	}
}
