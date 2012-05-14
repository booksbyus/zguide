/*
 *  Task worker2 in Scala
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.ZMQ

object taskwork2 {

  def main(args: Array[String]): Unit = {

    val context = ZMQ.context(1)

    val receiver = context.socket(ZMQ.PULL)
    receiver.connect("tcp://localhost:5557")

    val sender = context.socket(ZMQ.PUSH)
    sender.connect("tcp://localhost:5558")

    val controller = context.socket(ZMQ.SUB)
    controller.connect("tcp://localhost:5559")
    controller.subscribe("".getBytes)

    val items = context.poller(2)
    items.register(receiver, ZMQ.Poller.POLLIN)
    items.register(controller, ZMQ.Poller.POLLIN)

    var continue = true

    do {
      items.poll

      if (items.pollin(0)) {
        val message = new String(receiver.recv(0)).trim

        // Do the work
        Thread.sleep(message toLong)

        // Send results to sink
        sender.send(message.getBytes(), 0)

        // Simple progress indicator for the viewer
        print(".")
        Console.flush()
      }

      if (items.pollin(1)) {
        println()
        continue = false;
      }
    } while(continue)

    receiver.close()
    sender.close()

    controller.close()
    context.term()
  }
}
