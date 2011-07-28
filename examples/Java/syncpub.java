import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;


/**
 * Synchronized publisher.
 * 
 * Christophe Huntzinger <chuntzin_at_wanadoo.fr>
 */
public class syncpub{
	/**
	 * We wait for 10 subscribers
	 */
	protected static int SUBSCRIBERS_EXPECTED = 10;

	public static void main (String[] args) {
		Context context = ZMQ.context(1);

		//  Socket to talk to clients
		Socket publisher = context.socket(ZMQ.PUB);
		publisher.bind("tcp://*:5561");

		//  Socket to receive signals
		Socket syncservice = context.socket(ZMQ.REP);
		syncservice.bind("tcp://*:5562");

		//  Get synchronization from subscribers
		int subscribers = 0;
		while (subscribers < SUBSCRIBERS_EXPECTED) {
			//  - wait for synchronization request
			byte[] value = syncservice.recv(0);

			//  - send synchronization reply
			syncservice.send("".getBytes(), 0);
			subscribers++;
		}
		//  Now broadcast exactly 1M updates followed by END
		int update_nbr;
		for (update_nbr = 0; update_nbr < 1000000; update_nbr++){
			publisher.send("Rhubarb".getBytes(), 0);
		}

		publisher.send("END".getBytes(), 0);

		//  Give 0MQ/2.0.x time to flush output

		try {
			Thread.sleep (1);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}         

		// clean up
		publisher.close();
		syncservice.close();
		context.term();
	}
}