import java.util.Random;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Asynchronous client-to-server (DEALER to ROUTER)
 * 
 * This example contains clients and server in order to easily start and stop
 * the demo.
 * 
 * They are working independently and communicate only by using the tcp
 * connections. They conceptually act as separate processes.
 * 
 * @author Raphaël P. Barazzutti 
 */

// The client task connects to the server, then sends a request every second
// while printing incoming messages as they arrive
class ClientTask implements Runnable {
	final private int id;

	public ClientTask(int id) {
		this.id = id;
	}

	public void run() {
		Context context = ZMQ.context(1);
		Socket client = context.socket(ZMQ.DEALER);
		String identity = "worker-" + id;
		client.setIdentity(identity.getBytes());
		client.connect("tcp://localhost:5555");
		System.out.println("Client " + identity + " started");
		ZMQ.Poller poller = context.poller(1);
		poller.register(client, ZMQ.Poller.POLLIN);
		int requestNbr = 0;
		while (true)
			for (int i = 0; i < 100; i++) {
				poller.poll(10000);
				if (poller.pollin(0)) {
					byte msg[] = client.recv(0);
					System.out.println("Client " + identity + " received "
							+ new String(msg));
				}
				System.out.println("Req #" + (++requestNbr) + " sent");
				client.send(("request " + requestNbr).getBytes(), 0);
			}
	}

}

// The server worker receives messages and replies by re-sending them a random
// number of times (with random delays between replies)
class ServerWorker implements Runnable {
	private Context context;
	final private int id;

	public ServerWorker(Context context, int id) {
		super();
		this.id = id;
		this.context = context;
	}

	public void run() {
		Random randomGenerator = new Random();
		Socket worker = context.socket(ZMQ.DEALER);
		worker.connect("inproc://backend");
		System.out.println("Server worker " + id + " started");
		while (true) {
			byte id[] = worker.recv(0);
			byte msg[] = worker.recv(0);
			System.out.println("Server worker " + id + " received "
					+ new String(msg) + " from " + new String(id));
			// sending 0..4 replies
			for (int i = 0; i < randomGenerator.nextInt(5); i++) {
				try {
					// sleeping 1s or 1s/2 or 1s/3 .. 1s/9
					Thread.sleep(1000 / (1 + randomGenerator.nextInt(8)));
					worker.send(id, ZMQ.SNDMORE);
					worker.send(msg, 0);
				} catch (InterruptedException ie) {
					ie.printStackTrace();
				}
			}

		}

	}
}

// The server task uses a pool of workers to handle the messages coming from
// clients.
//
// The main server thread forwards messages between the front-end (connected to
// clients) and the back-end (connected to workers)
//
// The workers handle one request at a time, but a client might have its
// messages handled by more than one worker
class ServerTask implements Runnable {
	@Override
	public void run() {
		Context context = ZMQ.context(1);
		ZMQ.Socket frontend = context.socket(ZMQ.ROUTER);
		frontend.bind("tcp://*:5555");

		ZMQ.Socket backend = context.socket(ZMQ.DEALER);
		backend.bind("inproc://backend");

		for (int i = 0; i < 5; i++)
			(new Thread(new ServerWorker(context, i))).start();

		ZMQ.Poller poller = context.poller(2);
		poller.register(frontend, ZMQ.Poller.POLLIN);
		poller.register(backend, ZMQ.Poller.POLLIN);

		// It is possible to easily connect frontend to backend using a queue
		// device
		// ZMQQueue queue = new ZMQQueue(context, frontend, backend);
		// queue.run();
		//
		// Doing it manually gives a better understanding of the mechanisms
		// (it's a tuto) and might be useful in debugging
		while (true) {
			poller.poll();
			if (poller.pollin(0)) {
				byte[] id = frontend.recv(0);
				byte[] msg = frontend.recv(0);
				System.out.println("Server received " + new String(msg)
						+ " id " + new String(id));

				backend.send(id, ZMQ.SNDMORE);
				backend.send(msg, 0);
			}
			if (poller.pollin(1)) {
				byte[] id = backend.recv(0);
				byte[] msg = backend.recv(0);
				System.out.println("Sending to frontend " + new String(msg)
						+ " id " + new String(id));
				frontend.send(id, ZMQ.SNDMORE);
				frontend.send(msg, 0);
			}
		}
	}
}

// The main thread of the Java program, it starts three clients then starts a
// server
public class AsyncSrv {
	public static void main(String args[]) {
		// starting three clients
		for (int i = 0; i < 3; i++)
			(new Thread(new ClientTask(i))).start();

		// starting server
		new Thread(new ServerTask()).start();
	}
}
