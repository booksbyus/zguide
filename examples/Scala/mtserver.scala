/*
 *  Multithreaded Hello World server in Scala
 *
 *  @author Giovanni Ruggiero
 *  @email giovanni.ruggiero@gmail.com
 *
 */

import org.zeromq.ZMQ
import org.zeromq.ZMQQueue
import org.zeromq.ZMQ.{Context,Socket}

object mtserver {
  def main(args : Array[String]) {
		val context = ZMQ.context(1)
		val clients = context.socket(ZMQ.ROUTER)
		clients.bind ("tcp://*:5555")
		val workers = context.socket(ZMQ.DEALER)
		workers.bind ("inproc://workers")

		//  Launch pool of worker threads
		for (thread_nbr <- 1 to 5)  {
			val worker_routine = new Thread(){
				override def run(){
					val socket = context.socket(ZMQ.REP)
					socket.connect ("inproc://workers")

					while (true) {
						//  Wait for next request from client (C string)
						val request = socket.recv (0)
						println ("Received request: ["+new String(request,0,request.length-1)+"]")

						//  Do some 'work'
						try {
							Thread.sleep (1000)
						} catch  {
							case e: InterruptedException => e.printStackTrace()
						}

						//  Send reply back to client (C string)
						val reply = "World ".getBytes
						reply(reply.length-1) = 0 //Sets the last byte of the reply to 0
						socket.send(reply, 0)
					}
				}
			}
			worker_routine.start()
		}
		//  Connect work threads to client threads via a queue
		val zMQQueue = new ZMQQueue(context,clients, workers)
		zMQQueue run
	}
}
