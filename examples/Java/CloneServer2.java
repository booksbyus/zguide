import java.nio.ByteBuffer;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;
/**
 * Clone server Model Two
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class CloneServer2 {
	private static AtomicLong sequence = new AtomicLong();

	public void run() {
		Context ctx = ZMQ.context(1);
		Socket publisher = ctx.socket(ZMQ.PUB);
		publisher.bind("tcp://*:5557");

		List<Socket> socketList = ZHelper.buildZPipe(ctx);
		Socket updates = socketList.get(0);
		Socket peer = socketList.get(1);

		Thread t = new Thread(new StateManager(ctx, peer));
		t.start();

		Random random = new Random();
		while (true) {
			long currentSequenceNumber = sequence.incrementAndGet();
			int key = random.nextInt(10000);
			int body = random.nextInt(1000000);

			ByteBuffer b = ByteBuffer.allocate(4);
			b.asIntBuffer().put(body);

			KVSimple kvMsg = new KVSimple(key + "", currentSequenceNumber, b.array());
			kvMsg.send(publisher);
			kvMsg.send(updates);	//send a message to State Manager thead.
			
			System.out.println("sending "+ sequence);
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	
	
	public static void main(String[] args) {
		new CloneServer2().run();
	}
}
