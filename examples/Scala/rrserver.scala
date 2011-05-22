/*
 *  Hello World server in Scala
 *  Binds REP socket to tcp://localhost:5560
 *  Expects "Hello" from client, replies with "World"
 *
 *  @author Giovanni Ruggiero
 *  @email giovanni.ruggiero@gmail.com
 *
*/

import org.zeromq.ZMQ
import org.zeromq.ZMQ.{Context,Socket}

object rrserver {
  def main(args : Array[String]) {
    //  Prepare our context and socket
    val context = ZMQ.context(1)
    val receiver = context.socket(ZMQ.REP)
		receiver.connect("tcp://localhost:5560")

    while (true) {
      //  Wait for next request from client
      //  We will wait for a 0-terminated string (C string) from the client,
      //  so that this server also works with The Guide's C and C++ "Hello World" clients
      val request = receiver.recv (0)
      //  In order to display the 0-terminated string as a String,
      //  we omit the last byte from request
      println ("Received request: [" + new String(request,0,request.length-1)  //  Creates a String from request, minus the last byte
							 + "]")

      //  Do some 'work'
      try {
        Thread.sleep (1000)
      } catch  {
        case e: InterruptedException => e.printStackTrace()
      }

      //  Send reply back to client
      //  We will send a 0-terminated string (C string) back to the client,
      //  so that this server also works with The Guide's C and C++ "Hello World" clients
      val reply = "World ".getBytes
      reply(reply.length-1)=0 //Sets the last byte of the reply to 0
      receiver.send(reply, 0)
    }
  }
}
