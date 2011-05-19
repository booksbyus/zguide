/*
 *  Cross-connected ROUTER sockets addressing each other
 *
 *
 *  @Author:     Giovanni Ruggiero
 *  @Email:      giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import ZHelpers._

object rtrouter  {
  def main(args : Array[String]) {
		val ctx = ZMQ.context(1)
		val worker = ctx.socket(ZMQ.ROUTER)
    worker.setIdentity("WORKER" getBytes)
		worker.bind("tcp://*:5555")

		val server = ctx.socket(ZMQ.ROUTER)
    server.setIdentity("SERVER" getBytes)
		server.connect("tcp://localhost:5555")

    //  Wait for the worker to connect so that when we send a message
    //  with routing envelope, it will actually match the worker...
		Thread.sleep(1000)

		server.send("WORKER" getBytes, ZMQ.SNDMORE)
		server.send("" getBytes, ZMQ.SNDMORE)
		server.send("send to worker" getBytes,0)
		dump(worker)

		worker.send("SERVER" getBytes, ZMQ.SNDMORE)
		worker.send("" getBytes, ZMQ.SNDMORE)
		worker.send("send to server" getBytes,0)
		dump(server)

	}
}
