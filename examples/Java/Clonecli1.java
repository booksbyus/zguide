import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;

/**
 * Clone client model 1
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class Clonecli1 {
	private static Map<String, Kvsimple> kvMap = new HashMap<String, Kvsimple>();
	private static AtomicLong sequence = new AtomicLong();

	public void run() {
		ZContext ctx = new ZContext();
		Socket subscriber = ctx.createSocket(ZMQ.SUB);
		subscriber.connect("tcp://localhost:5556");
		subscriber.subscribe("".getBytes());

		while (true) {
			Kvsimple kvMsg = Kvsimple.recv(subscriber);
            if (kvMsg == null)
                break;

            Clonecli1.kvMap.put(kvMsg.getKey(), kvMsg);
            System.out.println("receiving " + kvMsg);
            sequence.incrementAndGet();
		}
        ctx.destroy();
	}

	public static void main(String[] args) {
		new Clonecli1().run();
	}
}
