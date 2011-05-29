/*
 *  Broker peering simulation (part 1)
 *  Prototypes the state flow
 *
 *
 *  @Author:     Giovanni Ruggiero
 *  @Email:      giovanni.ruggiero@gmail.com
*/

import org.zeromq.ZMQ
import ZHelpers._
import ClusterDns._

object peering1 {
	val Statefe = "statefe"
	val Statebe = "statebe"

  def main(args : Array[String]) {
    //  First argument is this broker's name
    //  Other arguments are our peers' names
    //
    if (args.length < 2) {
        println ("syntax: peering1 me {you}...")
        exit()
    }
    val self = args(0)
		implicit val dns = clusterDns
		implicit val host = self
    printf ("I: preparing broker at %s...\n", self);

		val rand = new java.util.Random(System.currentTimeMillis)
		val ctx = ZMQ.context(1)
		val statebe = ctx.socket(ZMQ.PUB)
		statebe.dnsBind(Statebe)
		val statefe = ctx.socket(ZMQ.SUB)
		statefe.subscribe("".getBytes)

		for (cluster <- (1 until args.length)) {
			printf ("I: connecting to state backend at '%s'\n", args(cluster))
			statefe.dnsConnect(args(cluster),Statefe)
		}
    //  Send out status messages to peers, and collect from peers
    //  The zmq_poll timeout defines our own heartbeating
		while (true) {
			val poller = ctx.poller(1)
			poller.register(statefe,ZMQ.Poller.POLLIN)
			poller.poll(1000000)
			if(poller.pollin(0)) {
				val msg = new ZMsg(statefe)
        printf ("%s - %s workers free\n", msg.addressToString, msg.bodyToString)
			} else {
        //  Send random value for worker availability
				val msg = new ZMsg(rand.nextInt(10).toString)
				msg.wrap(self getBytes)
				statebe.sendMsg(msg)
			}
		}
	}
}
