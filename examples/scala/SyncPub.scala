/*
 *
 * Synchronized publisher.
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import org.zeromq.ZMQ.Context
import org.zeromq.ZMQ.Socket

object SyncPub {
  def main(args : Array[String]) {
		/**
		 * We wait for 10 subscribers
		 */
		val SUBSCRIBERS_EXPECTED = 10

		val context = ZMQ.context(1)

		//  Socket to talk to clients
		val publisher = context.socket(ZMQ.PUB)
		publisher.bind("tcp://*:5561")

		//  Socket to receive signals
		val syncservice = context.socket(ZMQ.REP)
		syncservice.bind("tcp://*:5562")

		//  Get synchronization from subscribers
		for (subscribers <- 1 to SUBSCRIBERS_EXPECTED) {
			//  - wait for synchronization request
			var value = syncservice.recv(0)

			//  - send synchronization reply
			syncservice.send("".getBytes(), 0)
		}
		//  Now broadcast exactly 1M updates followed by END
		for (update_nbr <- 1 to 1000000){
			publisher.send("Rhubarb".getBytes(), 0)
		}

		publisher.send("END".getBytes(), 0)

		//  Give 0MQ/2.0.x time to flush output

		try {
			Thread.sleep (1000)
		} catch  {
			case e: InterruptedException => e.printStackTrace()
		}

		// clean up
		publisher.close()
		syncservice.close()
		context.term()
	}
}
