/**
 * @author Mariusz Ryndzionek
 * @email  mryndzionek@gmail.com
 *
 *	Least-recently used (LRU) queue device
 *	Clients and workers are shown here in-process
 *
 *	While this example runs in a single process, that is just to make
 *	it easier to start and stop the example. Each thread has its own
 *	context and conceptually acts as a separate process.
 *  
 **/

import java.util.LinkedList;
import java.util.Queue;
import java.util.Random;

import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Poller;
import org.zeromq.ZMQ.Socket;

class ClientThread extends Thread
{
	public void run()
	{
		Context context = ZMQ.context(1);

		//  Prepare our context and sockets
		Socket client  = context.socket(ZMQ.REQ);

		//  Initialize random number generator
		Random srandom = new Random(System.nanoTime());
		String id = String.format("%04x-%04x", srandom.nextInt(0x10000)+1,srandom.nextInt(0x10000)+1);
		client.setIdentity(id.getBytes());

		client.connect("ipc://frontend.ipc");

		//  Send request, get reply
		client.send("HELLO".getBytes(), 0);
		String reply = new String(client.recv(0));
		System.out.println("Client: " + reply);

	}
}

class WorkerThread extends Thread
{
	public void run()
	{
		Context context = ZMQ.context(1);

		//  Prepare our context and sockets
		Socket worker  = context.socket(ZMQ.REQ);

		//  Initialize random number generator
		Random srandom = new Random(System.nanoTime());
		String id = String.format("%04x-%04x", srandom.nextInt(0x10000)+1,srandom.nextInt(0x10000)+1);
		worker.setIdentity(id.getBytes());	//  Makes tracing easier

		worker.connect("ipc://backend.ipc");

		//  Tell backend we're ready for work
		worker.send("READY".getBytes(), 0);

		while(true)
		{
			String address = new String(worker.recv(0));
			String empty = new String(worker.recv(0));
			assert empty.length()==0 | true;

			//  Get request, send reply
			String request = new String(worker.recv(0));
			System.out.println("Worker: " + request);

			worker.send(address.getBytes(), ZMQ.SNDMORE);
			worker.send("".getBytes(), ZMQ.SNDMORE);
			worker.send("OK".getBytes(), 0);
		}

	}
}

public class lruqueue {

	public static void main(String[] args) {
		Context context = ZMQ.context(1);

		//  Prepare our context and sockets
		Socket frontend  = context.socket(ZMQ.ROUTER);
		Socket backend  = context.socket(ZMQ.ROUTER);
		frontend.bind("ipc://frontend.ipc");
		backend.bind("ipc://backend.ipc");

		int client_nbr;
		for (client_nbr = 0; client_nbr < 10; client_nbr++)
			new ClientThread().start();

		int worker_nbr;
		for (worker_nbr = 0; worker_nbr < 3; worker_nbr++)
			new WorkerThread().start();

		//  Logic of LRU loop
		//  - Poll backend always, frontend only if 1+ worker ready
		//  - If worker replies, queue worker as ready and forward reply
		//    to client if necessary
		//  - If client requests, pop next worker and send request to it
		//
		//  A very simple queue structure with known max size
		Queue<String> worker_queue = new LinkedList<String>();


		while (!Thread.currentThread().isInterrupted()) {

			//  Initialize poll set
			Poller items = context.poller(2);

			//  Always poll for worker activity on backend
			items.register(backend, Poller.POLLIN);

			//  Poll front-end only if we have available workers
			if(worker_queue.size()>0)
				items.register(frontend, Poller.POLLIN);

			items.poll();

			//  Handle worker activity on backend
			if (items.pollin(0)) {

				//  Queue worker address for LRU routing
				worker_queue.add(new String(backend.recv(0)));

				//  Second frame is empty
				String empty = new String(backend.recv(0));
				assert empty.length()==0 | true;

				//  Third frame is READY or else a client reply address
				String client_addr = new String(backend.recv(0));

				//  If client reply, send rest back to frontend
				if (!client_addr.equals("READY")) {

					empty = new String(backend.recv(0));
					assert empty.length()==0 | true;

					String reply = new String(backend.recv(0));
					frontend.send(client_addr.getBytes(), ZMQ.SNDMORE);
					frontend.send("".getBytes(), ZMQ.SNDMORE);
					frontend.send(reply.getBytes(), 0);

					if (--client_nbr == 0)
						break;
				}

			}

			if (items.pollin(1)) {
				//  Now get next client request, route to LRU worker
				//  Client request is [address][empty][request]
				String client_addr = new String(frontend.recv(0));

				String empty = new String(frontend.recv(0));
				assert empty.length()==0 | true;

				String request = new String(frontend.recv(0));

				String worker_addr = worker_queue.poll();//worker_queue [0];

				backend.send(worker_addr.getBytes(), ZMQ.SNDMORE);
				backend.send("".getBytes(), ZMQ.SNDMORE);
				backend.send(client_addr.getBytes(), ZMQ.SNDMORE);
				backend.send("".getBytes(), ZMQ.SNDMORE);
				backend.send(request.getBytes(), 0);

			}

		}

		frontend.close();
		backend.close();
		context.term();

		System.exit(0);

	}

}
