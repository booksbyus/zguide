//
//  Weather update server in Scala
//  Binds PUB socket to tcp://*:5556
//  Publishes random weather updates
//
//  author Giovanni Ruggiero
//  email giovanni.ruggiero@gmail.com

import java.util.Random
import org.zeromq.ZMQ

object wuserver {
  def main(args : Array[String]) {

		//  Prepare our context and publisher
		val context = ZMQ.context(1)
		val publisher = context.socket(ZMQ.PUB)

		publisher.bind("tcp://*:5556")

		//  Initialize random number generator
		val srandom = new Random(System.currentTimeMillis())
		while (true) {
			//  Get values that will fool the boss
			val zipcode: Integer = srandom.nextInt(100000) + 1
			val temperature: Integer = srandom.nextInt(215) - 80 + 1
			val relhumidity: Integer = srandom.nextInt(50) + 10 + 1

			//  Send message to all subscribers
			val update = String.format("%05d %d %d\u0000", zipcode, temperature, relhumidity);
			publisher.send(update.getBytes(), 0)
		}
	}
}
