/*
 *
 *  Weather proxy device in Scala
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.ZMQ

object wuproxy {

  def main(args: Array[String]) {

    //  Prepare our context and sockets
    var context = ZMQ.context(1)

    //  This is where the weather server sits
    var frontend = context.socket(ZMQ.SUB)
    frontend.connect("tcp://192.168.55.210:5556")

    //  This is our public endpoint for subscribers
    var backend = context.socket(ZMQ.PUB)
    backend.bind("tcp://10.1.1.0:8100")

    //  Subscribe on everything
    frontend.subscribe("".getBytes)

    //  Shunt messages out to our own subscribers
    while (!Thread.currentThread.isInterrupted) {
      var more = false
      do {
        var message = frontend.recv(0)
        more = frontend.hasReceiveMore
        backend.send(message, if (more) ZMQ.SNDMORE else 0)
      } while(more)
    }

    frontend.close()
    backend.close()
    context.term()
  }
}
