/*
 *  Asynchronous client-to-server (DEALER to BROKER)
 *
 *  While this example runs in a single process, that is just to make
 *  it easier to start and stop the example. Each task has its own
 *  context and conceptually acts as a separate process.
 *
 *  @Author:     Giovanni Ruggiero
 *  @Email:      giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import ZHelpers._

object asyncsrv  {
  //  ---------------------------------------------------------------------
  //  This is our client task
  //  It connects to the server, and then sends a request once per second
  //  It collects responses as they arrive, and it prints them out. We will
  //  run several client tasks in parallel, each with a different random ID.
	class ClientTask() extends Runnable {
		def run() {
			val ctx = ZMQ.context(1)
			val client = ctx.socket(ZMQ.DEALER)
      //  Generate printable identity for the client
			setID(client);
      val identity = new String(client getIdentity)
			// println(identity)
      client.connect("tcp://localhost:5570")
			val poller = ctx.poller(1)

			poller.register(client,ZMQ.Poller.POLLIN)
			var requestNbr = 0
			while (true) {
        //  Tick once per second, pulling in arriving messages
				for (centitick <- 1 to 100) {
					poller.poll(10000)
					if(poller.pollin(0)) {
						val msg = new ZMsg(client)
						printf("%s : %s\n", identity, msg.bodyToString)
					}
				}
				requestNbr += 1
				val msg = new ZMsg("request: %d" format requestNbr)
				client.sendMsg(msg)
			}
		}
	}
  //  ---------------------------------------------------------------------
  //  This is our server task
  //  It uses the multithreaded server model to deal requests out to a pool
  //  of workers and route replies back to clients. One worker can handle
  //  one request at a time but one client can talk to multiple workers at
  //  once.
	class ServerTask() extends Runnable {
		def run() {
			val Nworkers = 5
			val ctx = ZMQ.context(1)
			val frontend = ctx.socket(ZMQ.ROUTER)
			val backend = ctx.socket(ZMQ.DEALER)
			//  Frontend socket talks to clients over TCP
      frontend.bind("tcp://*:5570");
      //  Backend socket talks to workers over inproc
      backend.bind("inproc://backend");
      //  Launch pool of worker threads, precise number is not critical
			val workers = List.fill(Nworkers)(new Thread(new ServerWorker(ctx)))
			workers foreach (_.start)

      //  Connect backend to frontend via a queue device
      //  We could do this:
      //      zmq_device (ZMQ_QUEUE, frontend, backend);
      //  But doing it ourselves means we can debug this more easily

      //  Switch messages between frontend and backend
			val sockets = List(frontend,backend)
			val poller = ctx.poller(2)

			poller.register(frontend,ZMQ.Poller.POLLIN)
			poller.register(backend,ZMQ.Poller.POLLIN)

			while (true) {
				poller.poll
				if (poller.pollin(0)) {
					val msg = new ZMsg(frontend)
					println("Request from client: " + msg)
					backend.sendMsg(msg)
				}
				if (poller.pollin(1)) {
					val msg = new ZMsg(backend)
					println("Reply from worker: " + msg)
					frontend.sendMsg(msg)
				}
			}

		}
	}

  //  Accept a request and reply with the same text a random number of
  //  times, with random delays between replies.
  //
	class ServerWorker(ctx: ZMQ.Context) extends Runnable {
		def run() {
			val rand = new java.util.Random(System.currentTimeMillis)
			val worker = ctx.socket(ZMQ.DEALER)
      worker.connect("inproc://backend")
      while (true) {
        //  The DEALER socket gives us the address envelope and message
        val zmsg = new ZMsg(worker);
        //  Send 0..4 replies back
        val replies = rand.nextInt(5);
				for (reply <- 1 to replies) {
					Thread.sleep (rand.nextInt(1) * 1000)
					worker.sendMsg(zmsg)
				}
			}
		}
	}

  //  This main thread simply starts several clients, and a server, and then
  //  waits for the server to finish.
  //
  def main(args : Array[String]) {
		val Nclients = 3
		val clients = List.fill(Nclients)(new Thread(new ClientTask()))
		clients foreach (_.start)
		new Thread(new ServerTask()).start
	}
}
