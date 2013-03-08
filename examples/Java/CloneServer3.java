import java.nio.ByteBuffer;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;
/**
 * Clone server Model Three
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class CloneServer3 {
	private static AtomicLong sequence = new AtomicLong();
	Socket publisher;
	Socket updates;

	public void run() {
		Context ctx = ZMQ.context(1);
		publisher = ctx.socket(ZMQ.PUB);
		publisher.bind("tcp://*:5557");

		List<Socket> socketList = ZHelper.buildZPipe(ctx);
		updates = socketList.get(0);
		Socket peer = socketList.get(1);

		Thread t = new Thread(new StateManager3(ctx, peer, this));
		t.start();

		Random random = new Random();
		while (true) {
			int key = random.nextInt(10000);
			int body = random.nextInt(1000000);

			ByteBuffer b = ByteBuffer.allocate(4);
			b.asIntBuffer().put(body);

			send(key + "", b.array());
			System.out.println("sending " + sequence);
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public void send(String key, byte[] b) {
		KVSimple kvMsg = null;
		synchronized (publisher) {
			long currentSequenceNumber = sequence.incrementAndGet();
			kvMsg = new KVSimple(key, currentSequenceNumber, b);
			kvMsg.send(publisher);
		}
		kvMsg.send(updates); // send a message to State Manager thead.
	}

	public static void main(String[] args) {
		new CloneServer3().run();
	}
}
