/**
 * Custom routing Router to Dealer.
 * Scala version, based on the C version from
 * http://zguide.zeromq.org/chapter:all#toc45
 *
 * @author Giovanni Ruggiero
 * @email giovanni.ruggiero@gmail.com
 */

import org.zeromq.ZMQ

import java.util.Arrays
import java.util.Random


/**
 * Router-to-dealer custom routing demo.
 *
 * The router, in this case the main function, uses ROUTER.  The
 * dealers, in this case the two worker threads, use DEALER.
 */
object rtdealer  {
  val NOFLAGS = 0

  /**
   * Worker runnable consumes messages until it receives an END
   * message.
   */
  class Worker(name: String) extends Runnable {

    def run() {
      val context = ZMQ.context(1)
      val socket = context.socket(ZMQ.DEALER)
      socket.setIdentity(name.getBytes())
      socket.connect("tcp://localhost:5555")

      var total = 0
			var workload = ""
			do {
				 workload = new String(socket.recv(NOFLAGS))
				total += 1
			} while (!workload.equalsIgnoreCase("END"))
			printf( "Worker %s received %d messages.\n", name, total )
      socket.close
      context.term
    }
  }

  /* Random number generator to determine message distribution. */
  val rand = new Random

  def main(args : Array[String])  {
    val context = ZMQ.context(1)
    val socket = context.socket(ZMQ.ROUTER)
    socket.bind("tcp://*:5555")

    val workerA = new Thread(new Worker("A"))
    val workerB = new Thread(new Worker("B"))
    workerA.start()
    workerB.start()

    // Wait a second for the workers to connect their sockets.
    println("Workers started, sleeping 1 second for warmup.")
    Thread.sleep(1000)

    // Send 10 tasks, scattered to A twice as often as B.
		var address = Array[Byte]()
    for (i <- 1 to 10) {
      if (rand.nextInt() % 3 == 0) { // 1/3 to B.
        address = "B".getBytes()
      } else { // 2/3 to A.
        address = "A".getBytes()
			}
      socket.send(address, ZMQ.SNDMORE)
      socket.send("This is the workload.".getBytes, NOFLAGS)
    }
    socket.send("A".getBytes, ZMQ.SNDMORE)
    socket.send("END".getBytes, NOFLAGS)

    socket.send("B".getBytes, ZMQ.SNDMORE)
    socket.send("END".getBytes, NOFLAGS)

    socket.close
    context.term
  }

}
