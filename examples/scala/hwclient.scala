/*
 *
 *  Hello World client in Scala
 *  Connects REQ socket to tcp://localhost:5555
 *  Sends "Hello" to server, expects "World" back
 *
 *  @author Giovanni Ruggiero
 *  @email giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import org.zeromq.ZMQ.{Context,Socket}

object HelloWorldClient{
  def main(args : Array[String]) {
    //  Prepare our context and socket
    val context = ZMQ.context(1)
    val socket = context.socket(ZMQ.REQ)

    println("Connecting to hello world server...")
    socket.connect ("tcp://localhost:5555")

    //  Do 10 requests, waiting each time for a response
		for (request_nbr <- 1 to 10)  {
      //  Create a "Hello" message.
      //  Ensure that the last byte of our "Hello" message is 0 because
      //  our "Hello World" server is expecting a 0-terminated string:
			val request = "Hello ".getBytes()
      request(request.length-1)=0 //Sets the last byte to 0
      // Send the message
      println("Sending request " + request_nbr + "...") + request.toString
      socket.send(request, 0)

      //  Get the reply.
      val reply = socket.recv(0)
      //  When displaying reply as a String, omit the last byte because
      //  our "Hello World" server has sent us a 0-terminated string:
      println("Received reply " + request_nbr + ": [" + new String(reply,0,reply.length-1) + "]")
    }
  }
}
