/*
 *  Custom routing Router to Mama (ROUTER to REQ)
 *
 *  While this example runs in a single process, that is just to make
 *  it easier to start and stop the example. Each thread has its own
 *  context and conceptually acts as a separate process.
 *
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import ZHelpers._

object rtmama {

  class WorkerTask() extends Runnable {
    def run() {
			val rand = new java.util.Random(System.currentTimeMillis)
			val ctx = ZMQ.context(1)
			val worker = ctx.socket(ZMQ.REQ)
			//  We use a string identity for ease here
			setID(worker)
			// println(new String(worker.getIdentity))
			worker.connect("tcp://localhost:5555")
			var total = 0
			var workload = ""
      do {
				//  Tell the router we're ready for work
				worker.send("Ready".getBytes,0)
				workload = new String(worker.recv(0))
				Thread.sleep (rand.nextInt(1) * 1000)
				total += 1
				//  Get workload from router, until finished
			} while (!workload.equalsIgnoreCase("END"))
			printf("Processed: %d tasks\n", total)
		}
	}

  def main(args : Array[String]) {
		val NBR_WORKERS = 10
		val ctx = ZMQ.context(1)
		val client = ctx.socket(ZMQ.ROUTER)

		// Workaround to ckeck version >= 2.1
		assert(client.getType > -1)
		client.bind("tcp://*:5555")
		val workers = List.fill(NBR_WORKERS)(new Thread(new WorkerTask))
		workers foreach (_.start)

		for (i <- 1 to NBR_WORKERS * 10) {
			//  LRU worker is next waiting in queue
			val address = client.recv(0)
			val empty = client.recv(0)
			val ready = client.recv(0)

			client.send(address, ZMQ.SNDMORE)
			client.send("".getBytes, ZMQ.SNDMORE)
			client.send("This is the workload".getBytes,0)
		}

		//  Now ask mamas to shut down and report their results
		for (i <- 1 to NBR_WORKERS) {
			val address = client.recv(0)
			val empty = client.recv(0)
			val ready = client.recv(0)

			client.send(address, ZMQ.SNDMORE)
			client.send("".getBytes, ZMQ.SNDMORE)
			client.send("END".getBytes,0)
		}
	}
}
