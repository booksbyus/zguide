import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

/**
 * Clone server Model Three
 * @author Danish Shrestha <dshrestha06@gmail.com>
 *
 */
public class clonesrv3 {
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
		kvsimple kvMsg = null;
		synchronized (publisher) {
			long currentSequenceNumber = sequence.incrementAndGet();
			kvMsg = new kvsimple(key, currentSequenceNumber, b);
			kvMsg.send(publisher);
		}
		kvMsg.send(updates); // send a message to State Manager thead.
	}

	
	public static class StateManager3 implements Runnable {
		Context ctx;
		Socket pipe;
		Socket router;
		clonesrv3 server;
		private static Map<String, kvsimple> kvMap = new LinkedHashMap<String, kvsimple>();

		public StateManager3(Context ctx, Socket pipe, clonesrv3 server) {
			this.ctx = ctx;
			this.pipe = pipe;
			this.server = server;
		}

		@Override
		public void run() {
			this.pipe.send("READY".getBytes(), 0); // optional

			router = ctx.socket(ZMQ.XREP);
			router.bind("tcp://*:5556");
			
			Socket collector = ctx.socket(ZMQ.PULL);
			collector.bind("tcp://*:5558");

			Poller poller = ctx.poller(3);
			poller.register(this.pipe, ZMQ.Poller.POLLIN);
			poller.register(router, ZMQ.Poller.POLLIN);
			poller.register(collector, ZMQ.Poller.POLLIN);

			long stateSequence = 0;
			while (true) {
				poller.poll();

				// apply state updates from main thread
				if (poller.pollin(0)) {
					kvsimple kvMsg = kvsimple.recv(this.pipe);
					StateManager3.kvMap.put(kvMsg.getKey(), kvMsg);
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
				
				//collect updates
				if (poller.pollin(2)) {
					kvsimple kvMsg = kvsimple.recv(collector);
					this.server.send(kvMsg.getKey(), kvMsg.getBody());
					System.out.println("sending fired by alarm");
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
		new clonesrv3().run();
	}
}
