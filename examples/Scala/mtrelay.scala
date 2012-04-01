/*
 *  Multithreaded relay in Scala
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.ZMQ

object mtrelay {
  def main(args: Array[String]) {

    val context = ZMQ.context(1)

    //  Bind to inproc: endpoint, then start upstream thread
    var receiver = context.socket(ZMQ.PAIR)
    receiver.bind("inproc://step3")

    //  Step 2 relays the signal to step 3
    var step2 = new Thread() {
      override def run = {
        var receiver = context.socket(ZMQ.PULL)
        receiver.bind("inproc://step2")

        var step1 = new Thread {
          override def run = {
            var sender = context.socket(ZMQ.PUSH)
            sender.connect("inproc://step2")
            println("Step 1 ready, signaling step 2")
            sender.send("READY".getBytes, 0)
            sender.close()
          }
        }
        step1.start()

        var message = receiver.recv(0)

        var sender = context.socket(ZMQ.PAIR)
        sender.connect("inproc://step3")
        println ("Step 2 ready, signaling step 3");
        sender.send(message, 0)
        sender.close()
      }
    }

    step2.start()

    //  Wait for signal
    var message = receiver.recv(0)
    System.out.println("Test successful!")

    receiver.close()
  }
}
