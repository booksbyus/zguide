import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;
import org.zeromq.ZMQQueue;

/**
 * Simple message queuing broker
 * Same as request-reply broker but using QUEUE device.
 * 
 * Christophe Huntzinger <chuntz@laposte.net>
 */
public class msgqueue{

	public static void main (String[] args) {
		//  Prepare our context and sockets
		Context context = ZMQ.context(1);

		//  Socket facing clients
		Socket frontend = context.socket(ZMQ.ROUTER);
		frontend.bind("tcp://*:5559");

		//  Socket facing services
		Socket backend = context.socket(ZMQ.DEALER);
		backend.bind("tcp://*:5560");

		//  Start built-in device
		ZMQQueue queue = new ZMQQueue(context, frontend, backend);
		// have fun!
		
		//  We never get here but clean up anyhow
		frontend.close();
		backend.close();
		context.term();
	}
}
