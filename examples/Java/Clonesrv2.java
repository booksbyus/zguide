import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;
import java.util.Map.Entry;

import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZThread;
import org.zeromq.ZThread.IAttachedRunnable;

/**
 * Clone server Model Two
 * 
 * @author Danish Shrestha <dshrestha06@gmail.com>
 * 
 */
public class Clonesrv2 {

	public void run() {
		ZContext ctx = new ZContext();
		Socket publisher = ctx.createSocket(ZMQ.PUB);
		publisher.bind("tcp://*:5557");

		Socket updates = ZThread.fork(ctx, new StateManager());

		Random random = new Random();
        long sequence = 0;
		while (!Thread.currentThread().isInterrupted()) {
			long currentSequenceNumber = ++sequence;
			int key = random.nextInt(10000);
			int body = random.nextInt(1000000);

			ByteBuffer b = ByteBuffer.allocate(4);
			b.asIntBuffer().put(body);

			Kvsimple kvMsg = new Kvsimple(key + "", currentSequenceNumber, b.array());
			kvMsg.send(publisher);
			kvMsg.send(updates); // send a message to State Manager thead.

            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
            }

        }
        System.out.printf(" Interrupted\n%d messages out\n", sequence);

        ctx.destroy();
	}

	public static class StateManager implements IAttachedRunnable {
		private static Map<String, Kvsimple> kvMap = new LinkedHashMap<String, Kvsimple>();

		@Override
		public void run(Object[] args, ZContext ctx, Socket pipe) {
			pipe.send("READY"); // optional

			Socket snapshot = ctx.createSocket(ZMQ.ROUTER);
			snapshot.bind("tcp://*:5556");

			Poller poller = new ZMQ.Poller(2);
			poller.register(pipe, ZMQ.Poller.POLLIN);
			poller.register(snapshot, ZMQ.Poller.POLLIN);

			long stateSequence = 0;
			while (!Thread.currentThread().isInterrupted()) {
				if (poller.poll() < 0)
                    break;              //  Context has been shut down

				// apply state updates from main thread
				if (poller.pollin(0)) {
					Kvsimple kvMsg = Kvsimple.recv(pipe);
                    if (kvMsg == null)
                        break;
					StateManager.kvMap.put(kvMsg.getKey(), kvMsg);
					stateSequence = kvMsg.getSequence();
				}

				// execute state snapshot request
				if (poller.pollin(1)) {
					byte[] identity = snapshot.recv(0);
                    if (identity == null)
                        break;
					String request = new String(snapshot.recv(0));

                    if (!request.equals("ICANHAZ?")) {
                        System.out.println("E: bad request, aborting");
                        break;
                    }

                    Iterator<Entry<String, Kvsimple>> iter = kvMap.entrySet().iterator();
                    while (iter.hasNext()) {
                        Entry<String, Kvsimple> entry = iter.next();
                        Kvsimple msg = entry.getValue();
                        System.out.println("Sending message " + entry.getValue().getSequence());
                        this.sendMessage(msg, identity, snapshot);
                    }

                    // now send end message with sequence number
                    System.out.println("Sending state snapshot = " + stateSequence);
                    snapshot.send(identity, ZMQ.SNDMORE);
                    Kvsimple message = new Kvsimple("KTHXBAI", stateSequence, "".getBytes());
                    message.send(snapshot);
				}
			}
		}

		private void sendMessage(Kvsimple msg, byte[] identity, Socket snapshot) {
			snapshot.send(identity, ZMQ.SNDMORE);
			msg.send(snapshot);
		}
	}

	public static void main(String[] args) {
		new Clonesrv2().run();
	}
}
