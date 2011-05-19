/*
 * Durable subscriber
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 *
 */

import org.zeromq.ZMQ

object durasub {
  def main(args : Array[String]) {

		val context = ZMQ.context(1)

		// Connect our subscriber socket
		val subscriber = context.socket(ZMQ.SUB)

		// Synchronize with the publisher
		val sync = context.socket(ZMQ.PUSH)

		subscriber.setIdentity("Hello".getBytes)
		subscriber.subscribe("".getBytes)
		subscriber.connect("tcp://localhost:5565")
		sync.connect("tcp://localhost:5564")
		sync.send("".getBytes, 0)

		// Get updates, expect random Ctrl-C death
		var msg = ""
	  do {
			msg = new String(subscriber.recv(0))
			println(msg)
		} while (!msg.equalsIgnoreCase("END"))
	}
}
