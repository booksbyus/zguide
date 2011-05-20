/*
 *  Hello World client in Scala
 *  Connects REQ socket to tcp://localhost:5555
 *  Sends "Hello" to server, expects "World" back
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import org.zeromq.ZMQ.{Context,Socket}

object rrclient{
  def main(args : Array[String]) {
    //  Prepare our context and socket
    val context = ZMQ.context(1)
    val requester = context.socket(ZMQ.REQ)

    requester.connect ("tcp://localhost:5559")

		for (request_nbr <- 1 to 10)  {
			val request = "Hello ".getBytes()
      request(request.length-1)=0 //Sets the last byte to 0
      // Send the message
      println("Sending request " + request_nbr + "...") + request.toString
      requester.send(request, 0)

      //  Get the reply.
      val reply = requester.recv(0)
      //  When displaying reply as a String, omit the last byte because
      //  our "Hello World" server has sent us a 0-terminated string:
      println("Received reply " + request_nbr + ": [" + new String(reply,0,reply.length-1) + "]")
    }
  }
}
