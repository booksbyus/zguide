/*
 *  Multithreaded relay in Scala
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

object mtRelay {
  def main(args : Array[String]) {
		val context = ZMQ.context(1)

		//  Bind to inproc: endpoint, then start upstream thread
		val receiver = context.socket(ZMQ.PAIR)
		receiver.bind("inproc://step3")
		//  Step 2 relays the signal to step 3
		new Thread {
  		// val context1 = ZMQ.context(1)
			try {
				//  Bind to inproc: endpoint, then start upstream thread
				// println(context1)
			} catch {
				case e => e.printStackTrace()
			}
			// val receiver = context.socket(ZMQ.PAIR)
			receiver.bind("inproc://step2")
			new Thread{
				//  Signal downstream to step 2
				val sender = context.socket(ZMQ.PAIR)
				sender.connect("inproc://step2")
				sender.send("".getBytes(),0)
			}

			//  Wait for signal
			val message=receiver.recv(0)
			Thread.sleep (1000)
			//  Signal downstream to step 3
			val sender = context.socket(ZMQ.PAIR)
			sender.connect("inproc://step3")
			sender.send(message,0)
		}

		//  Wait for signal
		val message = receiver.recv(0)
		println ("Test successful!")
	}
}
