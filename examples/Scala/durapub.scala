/*
 * Publisher for durable subscriber
 *
 *  @author Giovanni Ruggiero
 *  @email giovanni.ruggiero@gmail.com
 *
 */

import org.zeromq.ZMQ

object durapub {
  def main(args : Array[String]) {

		val context = ZMQ.context(1)
		val publisher = context.socket(ZMQ.PUB)

		// Subscriber tells us when it's ready here
		val sync = context.socket(ZMQ.PULL)

		sync.bind("tcp://*:5564")

		// We send updates via this socket
		publisher.bind("tcp://*:5565")
		publisher setHWM 2
		// Wait for synchronization request
		sync recv 0

		// Now broadcast exactly 10 updates with pause
		for (i <- 1 to 10) {
			val msg = String.format("Update %d", i: Integer)
			publisher.send(msg.getBytes(), 0)
			Thread sleep 1000
		}
		publisher.send("END".getBytes(), 0)
		Thread sleep 1000 // Give 0MQ/2.0.x to flush output
	}
}
