/**
 *
 * Broker peering simulation (part 2)
 * Prototypes the request-reply flow
 *
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example. Each thread has its own
 * context and conceptually acts as a separate process.
 *
 *
 * @Author:     Giovanni Ruggiero
 * @Email:      giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import ZHelpers._
import ClusterDns._

object peering2 {
	val Localfe = "localfe"
	val Localbe = "localbe"
	val Cloudfe = "cloudfe"
	val Cloudbe = "cloudbe"

	implicit val dns = clusterDns

	//  Basic request-reply client using REQ socket
	//
	class ClientTask(host: String) extends Runnable {
		def run() {
			val ctx = ZMQ.context(1)
			val client = ctx.socket(ZMQ.REQ)
			setID(client);
			client.dnsConnect(host, Localfe)

			//  Send request, get reply
			client.send("HELLO".getBytes, 0)
			val reply = client.recv(0)
			printf("Client: %s\n", new String(reply))
		}
	}

	//  Worker using REQ socket to do LRU routing
	//
	class WorkerTask(host: String) extends Runnable {
		def run() {
			val ctx = ZMQ.context(1)
			val worker = ctx.socket(ZMQ.REQ)
			setID(worker);
			worker.dnsConnect(host, Localbe);
			//  Tell broker we're ready for work
			worker.send("READY".getBytes, 0);
			while (true) {
				//  Read and save all frames until we get an empty frame
				//  In this example there is only 1 but it could be more
				val msg = new ZMsg(worker)
				printf("Worker: %s\n", msg.bodyToString)
				msg.stringToBody("OK")
				msg.send(worker)
			}
		}
	}

  def main(args : Array[String]) {
		val NOFLAGS = 0

		//  Worker using REQ socket to do LRU routing
		//
		val NbrClients = 10;
		val NbrWorkers = 3;

    //  First argument is this broker's name
    //  Other arguments are our peers' names
    //
    if (args.length < 2) {
      println ("syntax: peering2 me {you}...")
      exit()
    }
    val self = args(0)
		implicit val host = self
    printf ("I: preparing broker at %s...\n", self);

		val rand = new java.util.Random(System.currentTimeMillis)
		val ctx = ZMQ.context(1)
    //  Bind cloud frontend to endpoint
		val cloudfe = ctx.socket(ZMQ.ROUTER)
		cloudfe.setIdentity(self getBytes)
		cloudfe.dnsBind(Cloudfe)

		val cloudbe = ctx.socket(ZMQ.ROUTER)
		cloudbe.setIdentity(self getBytes)

		for (cluster <- (1 until args.length)) {
			printf ("I: connecting to cloud frontend at '%s'\n", args(cluster))
			cloudbe.dnsConnect(args(cluster),Cloudbe)
		}
    //  Prepare local frontend and backend
		val localfe = ctx.socket(ZMQ.ROUTER)
		val localbe = ctx.socket(ZMQ.ROUTER)
		localfe.dnsBind(Localfe)
		localbe.dnsBind(Localbe)

    println ("Press Enter when all brokers are started: ");
		readChar

    //  Start local clients
		val clients = List.fill(NbrClients)(new Thread(new ClientTask(self)))
		clients foreach (_.start)

    //  Start local workers
		val workers = List.fill(NbrWorkers)(new Thread(new WorkerTask(self)))
		workers foreach (_.start)

    //  Interesting part
    //  -------------------------------------------------------------
    //  Request-reply flow
    //  - Poll backends and process local/cloud replies
    //  - While worker available, route localfe to local or cloud

    //  Queue of available workers

		val workerQueue = scala.collection.mutable.Queue[Array[Byte]]()
		val backends = ctx.poller(2)

		backends.register(localbe,ZMQ.Poller.POLLIN)
		backends.register(cloudbe,ZMQ.Poller.POLLIN)

		var capacity = 0
		while (true) {
			//  If we have no workers anyhow, wait indefinitely
			val timeout = if (capacity > 0) {1000000} else {-1}
			val ret = backends.poll(timeout)
			//  Handle reply from local worker
			var msg = new ZMsg()
			if (backends.pollin(0)) {
				msg = new ZMsg(localbe)
        val workerAddr = msg.unwrap
				assert(capacity < NbrWorkers)
				//  Use worker address for LRU routing
				workerQueue.enqueue(workerAddr)
				capacity += 1
				//  Address is READY or else a client reply address
			} else {
        //  Or handle reply from peer broker
				if (backends.pollin(1)) {
				  msg = new ZMsg(cloudbe)
				}
			}
      //  Route reply to cloud if it's addressed to a broker
			if (msg != null) {
				for (cluster <- (1 until args.length)) {
				  if (new String(msg.address) == cluster) {
					  cloudfe.sendMsg(msg)
					}
				}
			}
			//  Route reply to client if we still need to
			if (msg != null) {
				localfe.sendMsg(msg)
			}
      //  Now route as many clients requests as we can handle
			while (capacity > 0) {
				val frontends = ctx.poller(2)

				frontends.register(localfe,ZMQ.Poller.POLLIN)
				frontends.register(cloudfe,ZMQ.Poller.POLLIN)
				frontends.poll
        var reroutable = 0
        //  We'll do peer brokers first, to prevent starvation
				if (frontends.pollin(1)) {
					msg = new ZMsg(cloudfe)
					reroutable = 0
				} else if (frontends.pollin(0)) {
					msg = new ZMsg(localfe)
					reroutable = 1
				}
				//  If reroutable, send to cloud 20% of the time
        //  Here we'd normally use cloud status information
				val rand = new java.util.Random
				if (reroutable > 0 && args.length > 1 && rand.nextInt() % 5 == 0) {
					//  Route to random broker peer
					val randomPeer = rand.nextInt(args.length - 1) + 1
					msg.wrap(args(randomPeer) getBytes)
					cloudbe.sendMsg(msg)
				} else {
					msg.wrap(workerQueue(0))
					localbe.sendMsg(msg)
					workerQueue.dequeue
					capacity -= 1
				}
			}
		}
	}
}
