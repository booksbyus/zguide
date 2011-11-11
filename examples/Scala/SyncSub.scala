/*
 * Synchronized subscriber
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import org.zeromq.ZMQ.Context
import org.zeromq.ZMQ.Socket

object SyncSub {
  def main(args : Array[String]) {
		val context = ZMQ.context(1)

		//  First, connect our subscriber socket
		val subscriber = context.socket(ZMQ.SUB)
		subscriber.connect("tcp://localhost:5561")
		subscriber.subscribe("".getBytes())

		//  Second, synchronize with publisher
		val syncclient = context.socket(ZMQ.REQ)
		subscriber.connect("tcp://localhost:5562")

		//  - send a synchronization request
		syncclient.send("".getBytes(), 0)

		//  - wait for synchronization reply
		val value = syncclient.recv(0)

		//  Third, get our updates and report how many we got
		var update_nbr = 0
		var string = ""
		do {
			var stringValue = subscriber.recv(0)
			string  = new String(stringValue)
			update_nbr = update_nbr + 1
		} while (string != "END")
		println("Received "+update_nbr+" updates.")

		subscriber.close()
		syncclient.close()
		context.term()
	}
}
