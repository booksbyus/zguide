/*
 *  Least-recently used (LRU) queue device
 *  Clients and workers are shown here in-process
 *
 *  While this example runs in a single process, that is just to make
 *  it easier to start and stop the example. Each thread has its own
 *  context and conceptually acts as a separate process.
 *
 *
 *  Author:     Giovanni Ruggiero
 *  Email:      giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import ZHelpers._

//  Basic request-reply client using REQ socket
//
class ClientTask() extends Runnable {
	def run() {
		val ctx = ZMQ.context(1)
		val client = ctx.socket(ZMQ.REQ)
		setID(client)
		client.connect("tcp://localhost:5555")

		//  Send request, get reply
		client.send("HELLO".getBytes, 0)
		val reply = client.recv(0)
		printf("Client: %s\n", new String(reply))
	}
}

//  Worker using REQ socket to do LRU routing
//
class WorkerTask() extends Runnable {
	def run() {
		// println("worker started")
		// Thread.sleep(1000)
		val ctx = ZMQ.context(1)
		val worker = ctx.socket(ZMQ.REQ)
		setID(worker)
		worker.connect("tcp://localhost:5556")
		//  Tell broker we're ready for work
		worker.send("READY".getBytes, 0)
		while (true) {
			//  Read and save all frames until we get an empty frame
			//  In this example there is only 1 but it could be more
			val address = worker.recv(0)
			val empty = worker.recv(0)

			//  Get request, send reply
			val request = worker.recv(0)
			printf("Worker: %s\n", new String(request))

			worker.send(address, ZMQ.SNDMORE)
			worker.send("".getBytes, ZMQ.SNDMORE)
			worker.send("OK".getBytes, 0)
		}
	}
}

object lruqueue   {
  def main(args : Array[String]) {
		val NOFLAGS = 0

		//  Worker using REQ socket to do LRU routing
		//
		val NBR_CLIENTS = 10
		val NBR_WORKERS = 3

		//  Prepare our context and sockets
		val ctx = ZMQ.context(1)
		val frontend = ctx.socket(ZMQ.ROUTER)
		val backend = ctx.socket(ZMQ.ROUTER)

		frontend.bind("tcp://*:5555")
		backend.bind("tcp://*:5556")

		val clients = List.fill(NBR_CLIENTS)(new Thread(new ClientTask))
		clients foreach (_.start)

		val workers = List.fill(NBR_WORKERS)(new Thread(new WorkerTask))
		workers foreach (_.start)

		//  Logic of LRU loop
		//  - Poll backend always, frontend only if 1+ worker ready
		//  - If worker replies, queue worker as ready and forward reply
		//    to client if necessary
		//  - If client requests, pop next worker and send request to it
		val workerQueue = scala.collection.mutable.Queue[Array[Byte]]()
		var availableWorkers = 0

		val poller = ctx.poller(2)

		// Always poll for worker activity on backend
		poller.register(backend,ZMQ.Poller.POLLIN)

		// Poll front-end only if we have available workers
		poller.register(frontend,ZMQ.Poller.POLLIN)

		var clientNbr = NBR_CLIENTS
		while (true) {
			poller.poll

			if(poller.pollin(0) && clientNbr > 0) {
				val workerAddr = backend.recv(NOFLAGS)
				assert (availableWorkers < NBR_WORKERS)
				availableWorkers += 1

				//  Queue worker address for LRU routing
				workerQueue.enqueue(workerAddr)
				//  Second frame is empty
				var empty = backend.recv(NOFLAGS)
				assert(new String(empty) == "")
				//  Third frame is READY or else a client reply address
				val clientAddr = backend.recv(NOFLAGS)
				if (!new String(clientAddr).equals("READY")) {
					val reply = backend.recv(NOFLAGS)
					frontend.send(clientAddr, ZMQ.SNDMORE)
					frontend.send("".getBytes, ZMQ.SNDMORE)
					frontend.send(reply, NOFLAGS)
					clientNbr -=1 //  Exit after N messages
				}
			}
			if(availableWorkers > 0 && poller.pollin(1)) {
				//  Now get next client request, route to LRU worker
				//  Client request is [address][empty][request]
				val clientAddr = frontend.recv(NOFLAGS)
				val empty = frontend.recv(NOFLAGS)
				val request = frontend.recv(NOFLAGS)

				backend.send(workerQueue.dequeue, ZMQ.SNDMORE)
				backend.send("".getBytes, ZMQ.SNDMORE)
				backend.send(clientAddr, ZMQ.SNDMORE)
				backend.send("".getBytes, ZMQ.SNDMORE)
				backend.send(request, NOFLAGS)
				availableWorkers -= 1
			}

		}
	}
}
