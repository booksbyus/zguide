/*
 *  Reading from multiple sockets in Scala
 *  This version uses ZMQ.Poller
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

object mspoller {
  def main(args : Array[String]) {

		val context = ZMQ.context(1)

		// Connect to task ventilator
		val receiver = context.socket(ZMQ.PULL)
		receiver.connect("tcp://localhost:5557")

		//  Connect to weather server
		val subscriber = context.socket(ZMQ.SUB)
		subscriber.connect("tcp://localhost:5556")
		subscriber.subscribe("10001 ".getBytes())

		//  Initialize poll set
		val items = context.poller(2)
		items.register(receiver, 0)
		items.register(subscriber, 0)

		//  Process messages from both sockets
		while (true) {
			items.poll()
			if (items.pollin(0)) {
				val message0 = receiver.recv(0)
				//  Process task
			}
			if (items.pollin(1)) {
				val message1 = subscriber.recv(0)
				//  Process weather update
			}
		}
	}
}
