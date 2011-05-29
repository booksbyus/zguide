/*
 * Pubsub envelope subscriber
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

object psenvsub {
  def main(args : Array[String]) {

		// Prepare our context and subscriber
		val context = ZMQ.context(1)
		val subscriber = context.socket(ZMQ.SUB)

		subscriber.connect("tcp://localhost:5563")
		subscriber.subscribe("B".getBytes())
		while (true) {
			// Read envelope with address
			val address = new String(subscriber.recv(0))
			// Read message contents
			val contents = new String(subscriber.recv(0))
			println(address + " : " + contents)
		}
	}
}
