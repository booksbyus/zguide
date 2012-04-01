/*
 *
 *  Interrupt in Scala
 *  Shows how to handle Ctrl-C
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.{ZMQException, ZMQ}

object interrupt {

  def main(args: Array[String]) {
    val context: ZMQ.Context = ZMQ.context(1)

    val zmqThread = new Thread(new Runnable {
      def run() {
        var socket = context.socket(ZMQ.REP)
        socket.bind("tcp://*:5555")

        while (!Thread.currentThread.isInterrupted) {
          try {
            socket.recv(0)
          } catch {
            case e: ZMQException if ZMQ.Error.ETERM.getCode == e.getErrorCode =>
              Thread.currentThread.interrupt()
            case e => throw e
          }
        }

        socket.close()
        println("ZMQ socket shutdown complete")
      }
    })

    sys.addShutdownHook({
      println("ShutdownHook called")
      context.term()
      zmqThread.interrupt()
      zmqThread.join
    })

    zmqThread.start()
  }
}
