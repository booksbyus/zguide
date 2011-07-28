import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Synchronized subscriber.
 *
 * Christophe Huntzinger <chuntzin_at_wanadoo.fr>
 *
 */
public class syncsub{
	public static void main (String[] args) {
		Context context = ZMQ.context(1);

		//  First, connect our subscriber socket
		Socket subscriber = context.socket(ZMQ.SUB);
		subscriber.connect("tcp://localhost:5561");
		subscriber.subscribe("".getBytes());

		//  Second, synchronize with publisher
		Socket syncclient = context.socket(ZMQ.REQ);
		syncclient.connect("tcp://localhost:5562");

		//  - send a synchronization request
		syncclient.send("".getBytes(), 0);

		//  - wait for synchronization reply
		byte[] value = syncclient.recv(0);

		//  Third, get our updates and report how many we got
		int update_nbr = 0;
		while (true) {
			byte[] stringValue = subscriber.recv(0);
			String string  = new String(stringValue);
			if (string.equals("END")) {
				break;
			}
			update_nbr++;
		}
		System.out.println("Received "+update_nbr+" updates.");

		subscriber.close();
		syncclient.close();
		context.term();
	}
}