/*
 *
 *  Reading from multiple sockets in Scala
 *  This version uses a simple recv loop
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

object msreader {
  def main(args : Array[String]) {

		//  Prepare our context and sockets
		val context = ZMQ.context(1)

		// Connect to task ventilator
		val receiver = context.socket(ZMQ.PULL)
		receiver.connect("tcp://localhost:5557")

		//  Connect to weather server
		val subscriber = context.socket(ZMQ.SUB)
		subscriber.connect("tcp://localhost:5556")
		subscriber.subscribe("10001 ".getBytes())

		//  Process messages from both sockets
		//  We prioritize traffic from the task ventilator
		while (true) {
			//  Process any waiting tasks
			val task = receiver.recv(ZMQ.NOBLOCK)
			while(task != null) {
				//  process task
			}
			//  Process any waiting weather updates
			val update = subscriber.recv(ZMQ.NOBLOCK)
			while (update != null) {
				//  process weather update
			}
			//  No activity, so sleep for 1 msec
			Thread.sleep(1)
		}
	}
}
