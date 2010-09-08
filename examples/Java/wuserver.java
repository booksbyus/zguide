import java.util.Random;
import org.zeromq.ZMQ;

//
//  Weather update server in Java
//  Binds PUB socket to tcp://*:5556
//  Publishes random weather updates
//
//  Nicola Peduzzi <thenikso@gmail.com>
//
public class wuserver {

	public static void main(String[] args) {
		//  Prepare our context and publisher
		ZMQ.Context context = ZMQ.context(1);

		ZMQ.Socket publisher = context.socket(ZMQ.PUB);
		publisher.bind("tcp://*:5556");
		publisher.bind("ipc://weather");

		//  Initialize random number generator
		Random srandom = new Random(System.currentTimeMillis());
		while (true) {
			//  Get values that will fool the boss
			int zipcode, temperature, relhumidity;
			zipcode = srandom.nextInt(100000) + 1;
			temperature = srandom.nextInt(215) - 80 + 1;
			relhumidity = srandom.nextInt(50) + 10 + 1;

			//  Send message to all subscribers
			String update = String.format("%05d %d %d\u0000", zipcode, temperature, relhumidity);
			publisher.send(update.getBytes(), 0);
		}
	}
}
