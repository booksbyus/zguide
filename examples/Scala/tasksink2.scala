/*
 *
 *  Task sink2 in Scala
 *  Binds PULL socket to tcp://localhost:5558
 *  Collects results from workers via that socket
 *  publishes a kill signal to tcp://localhost:5559 when the results have been processed.
 *
 * @author Vadim Shalts
 * @email vshalts@gmail.com
*/

import org.zeromq.ZMQ

object tasksink2 {

  def main(args: Array[String]) {

    //  Prepare our context and socket
    val context = ZMQ.context(1)

    val receiver = context.socket(ZMQ.PULL)
    receiver.bind("tcp://*:5558")

    val controller = context.socket(ZMQ.PUB);
    controller.bind("tcp://*:5559");

    //  Wait for start of batch
    receiver.recv(0)

    //  Wait for start of batch
    val string = new String(receiver.recv(0))

    //  Start our clock now
    val tstart = System.currentTimeMillis

    for (task_nbr <- 1 to 100) {
      val string = new String(receiver.recv(0)).trim
      if ((task_nbr / 10) * 10 == task_nbr) {
        print(":")
      } else {
        print(".")
      }
      Console.flush()
    }

    // Calculate and report duration of batch
    val tend = System.currentTimeMillis

    println("Total elapsed time: " + (tend - tstart) + " msec")

    // Send the kill signal to the workers
    controller.send("KILL".getBytes(), 0)

    // Give it some time to deliver
    Thread.sleep(1)

    controller.close()
    receiver.close()

    context.term()
  }
}
