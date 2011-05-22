/*
 * Pubsub envelope publisher
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

object psenvpub {
  def main(args : Array[String]) {

		// Prepare our context and publisher
		val context = ZMQ.context(1)
		val publisher = context.socket(ZMQ.PUB)

		publisher.bind("tcp://*:5563")
		while (true) {
			// Write two messages, each with an envelope and content
			publisher.send("A".getBytes(), ZMQ.SNDMORE)
			publisher.send("We don't want to see this".getBytes(), 0)
			publisher.send("B".getBytes(), ZMQ.SNDMORE)
			publisher.send("We would like to see this".getBytes(), 0)
			Thread.sleep(1000)
		}
	}
}
