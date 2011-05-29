/*
 *  Custom routing Router to Papa (ROUTER to REP)
 *
 *
 *  @Author:     Giovanni Ruggiero
 *  @Email:      giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ
import ZHelpers._

object rtpapa  {
  def main(args : Array[String]) {
		val NOFLAGS = 0
		//  We will do this all in one thread to emphasize the sequence
		//  of events...
		val ctx = ZMQ.context(1)
		val client = ctx.socket(ZMQ.ROUTER)
		val worker = ctx.socket(ZMQ.REP)

		client.bind("inproc://routing")
		worker.setIdentity("A".getBytes)
		worker.connect("inproc://routing")

		//  Wait for the worker to connect so that when we send a message
		//  with routing envelope, it will actually match the worker...
		Thread.sleep(1000)

		//  Send papa address, address stack, empty part, and request
		client.send("A".getBytes, ZMQ.SNDMORE)
		client.send("address 3".getBytes, ZMQ.SNDMORE)
		client.send("address 2".getBytes, ZMQ.SNDMORE)
		client.send("address 1".getBytes, ZMQ.SNDMORE)
		client.send("".getBytes, ZMQ.SNDMORE)
		client.send("This is the workload".getBytes, NOFLAGS)

		//  Worker should get just the workload
		dump(worker)

		//  We don't play with envelopes in the worker
		worker.send("This is the reply".getBytes, NOFLAGS)

		//  Now dump what we got off the ROUTER socket...
		dump(client)
	}
}
