import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;


/**
 * Clone server Model Two
 * 
 * @author Danish Shrestha <dshrestha06@gmail.com>
 * 
 */
public class clonesrv2 {
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

			kvsimple kvMsg = new kvsimple(key + "", currentSequenceNumber, b.array());
			kvMsg.send(publisher);
			kvMsg.send(updates); // send a message to State Manager thead.

			System.out.println("sending " + sequence);
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public static class StateManager implements Runnable {
		Context ctx;
		Socket pipe;
		Socket router;
		private static Map<String, kvsimple> kvMap = new LinkedHashMap<String, kvsimple>();

		public StateManager(Context ctx, Socket pipe) {
			this.ctx = ctx;
			this.pipe = pipe;
		}

		@Override
		public void run() {
			this.pipe.send("READY".getBytes(), 0); // optional

			router = ctx.socket(ZMQ.XREP);
			router.bind("tcp://*:5556");

			Poller poller = ctx.poller(2);
			poller.register(this.pipe, ZMQ.Poller.POLLIN);
			poller.register(router, ZMQ.Poller.POLLIN);

			long stateSequence = 0;
			while (true) {
				poller.poll();

				// apply state updates from main thread
				if (poller.pollin(0)) {
					kvsimple kvMsg = kvsimple.recv(this.pipe);
					StateManager.kvMap.put(kvMsg.getKey(), kvMsg);
					stateSequence = kvMsg.getSequence();
				}

				// execute state snapshot request
				if (poller.pollin(1)) {
					byte[] identity = router.recv(0);
					String request = new String(router.recv(0));

					if (request.equals("ICANHAZ?")) {
						System.out.println("EQ");

						Iterator<Entry<String, kvsimple>> iter = kvMap.entrySet().iterator();
						while (iter.hasNext()) {
							Entry<String, kvsimple> entry = iter.next();
							kvsimple msg = entry.getValue();
							System.out.println("Sending message " + entry.getValue().getSequence());
							this.sendMessage(msg, identity);
						}

						// now send end message with sequence number
						System.out.println("Sending state snapshot = " + stateSequence);
						router.send(identity, ZMQ.SNDMORE);
						kvsimple message = new kvsimple("KTHXBAI", stateSequence, "".getBytes());
						message.send(router);
					}
				}
			}
		}

		private void sendMessage(kvsimple msg, byte[] identity) {
			System.out.println("Sending via router");
			this.router.send(identity, ZMQ.SNDMORE);
			msg.send(this.router);
		}
	}

	public static void main(String[] args) {
		new clonesrv2().run();
	}
}
