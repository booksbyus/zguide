/*
 *  Task worker in Scala
 *  Connects PULL socket to tcp://localhost:5557
 *  Collects workloads from ventilator via that socket
 *  Connects PUSH socket to tcp://localhost:5558
 *  Sends results to sink via that socket
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ

object taskwork {
  def main(args : Array[String]) {
		val context = ZMQ.context(1)

		//  Socket to receive messages on
		val receiver = context.socket(ZMQ.PULL)
		receiver.connect("tcp://localhost:5557")

		//  Socket to send messages to
		val sender = context.socket(ZMQ.PUSH)
		sender.connect("tcp://localhost:5558")

		//  Process tasks forever
		while (true) {
			val string = new String(receiver.recv(0)).trim()
			val nsec = string.toLong * 1000
			//  Simple progress indicator for the viewer
			System.out.flush()
			print(string + '.')

			//  Do the work
			Thread.sleep(nsec)

			//  Send results to sink
			sender.send("".getBytes(), 0)
		}
	}
}
