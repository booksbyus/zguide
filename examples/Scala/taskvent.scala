/*
 *
 *  Task ventilator in Scala
 *  Binds PUSH socket to tcp://localhost:5557
 *  Sends batch of tasks to workers via that socket
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
*/

import java.util.Random
import org.zeromq.ZMQ

object taskvent {
  def main(args : Array[String]) {

		val context = ZMQ.context(1)

		//  Socket to send messages on
		val sender = context.socket(ZMQ.PUSH)
		sender.bind("tcp://*:5557")

		println("Press Enter when the workers are ready: ")
		System.in.read()
		println("Sending tasks to workers...\n")

		//  The first message is "0" and signals start of batch
		sender.send("0\u0000".getBytes(), 0)

		//  Initialize random number generator
		val srandom = new Random(System.currentTimeMillis())

		//  Send 100 tasks
		var total_msec = 0     //  Total expected cost in msecs
		for (i <- 1 to 100 ) {
			//  Random workload from 1 to 100msecs
			val workload = srandom.nextInt(100) + 1
			total_msec += workload
			print(workload + ".")
			val string = String.format("%d\u0000", workload.asInstanceOf[Integer] )
			sender.send(string.getBytes(), 0)
		}
		println("Total expected cost: " + total_msec + " msec")

		Thread.sleep(1000)              //  Give 0MQ time to deliver
	}
}
