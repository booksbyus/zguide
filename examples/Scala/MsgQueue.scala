s/**
 * Simple message queuing broker
 * Same as request-reply broker but using QUEUE device.
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import org.zeromq.ZMQ.Context
import org.zeromq.ZMQ.Socket
import org.zeromq.ZMQQueue

object MsgQueue {
  def main(args : Array[String]) {

		//  Prepare our context and sockets
		val context = ZMQ.context(1)

		//  Socket facing clients
		val frontend = context.socket(ZMQ.ROUTER)
		frontend.bind("tcp://*:5559")

		//  Socket facing services
		val backend = context.socket(ZMQ.DEALER)
		backend.bind("tcp://*:5560")

		//  Start built-in device
		val queue = new ZMQQueue(context, frontend, backend)
		// have fun!

		//  We never get here but clean up anyhow
		frontend close

		backend close

		context term
	}
}
