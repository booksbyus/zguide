/*
 * Simple request-reply broker
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 *
 */

import org.zeromq.ZMQ

object rrbroker  {
  def main(args : Array[String]) {
		//  Prepare our context and sockets
		val context = ZMQ.context(1)

		val frontend = context.socket(ZMQ.ROUTER)
		val backend  = context.socket(ZMQ.DEALER)
		frontend.bind("tcp://*:5559")
		backend.bind("tcp://*:5560")

		System.out.println("launch and connect broker.")

		//  Initialize poll set
		val items = context.poller(2)
		items.register(frontend, 1)
		items.register(backend, 1)

		var more = false

		//  Switch messages between sockets
		while (!Thread.currentThread().isInterrupted()) {
			//  poll and memorize multipart detection
			items.poll()

			if (items.pollin(0)) {
			do {
				// receive message
				val message = frontend.recv(0)
				more = frontend.hasReceiveMore

				// Broker it
				if (more)
					backend.send(message, ZMQ.SNDMORE)
				else
					backend.send(message, 0)
			} while (more)
			}
			if (items.pollin(1)) {
			do {
				// receive message
				val message = backend.recv(0)
				more = backend.hasReceiveMore()
				// Broker it
				if (more)
					frontend.send(message, ZMQ.SNDMORE)
				else
					frontend.send(message, 0)
			} while (more)
			}
		}
		//  We never get here but clean up anyhow
		frontend.close()
		backend.close()
		context.term()
	}
}
