import org.zeromq.ZMQ;
import java.util.Random;

/*
 * Lazy Pirate server
 * @author Zac Li
 * @email zac.li.cmu@gmail.com 
 */

object lpserver{

    def main (args : Array[String]) {
        val rand = new Random(System.nanoTime())

        val context = ZMQ.context(1)
        val server = context.socket(ZMQ.REP)
        server.bind("tcp://*:5555")

        val cycles = 0;
        while (true) {
            val request = server.recvStr()
            cycles++

            //  Simulate various problems, after a few cycles
            if (cycles > 3 && rand.nextInt(3) == 0) {
                println("I: simulating a crash")
                break
            } else if (cycles > 3 && rand.nextInt(3) == 0) {
                println("I: simulating CPU overload")
                Thread.sleep(2000)
            }
            println(f"I: normal request (%s)\n", request)
            Thread.sleep(1000)
            server.send(request)
        }

        server close()
        context term()
    }
}