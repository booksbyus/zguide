import java.util.StringTokenizer;
import org.zeromq.ZMQ;

//
//  Weather update client in Java
//  Connects SUB socket to tcp://localhost:5556
//  Collects weather updates and finds avg temp in zipcode
//
//  Nicola Peduzzi <thenikso@gmail.com>
//
public class wuclient {

	public static void main(String[] args) {
		ZMQ.Context context = ZMQ.context(1);

		//  Socket to talk to server
		System.out.println("Collecting updates from weather server...");
		ZMQ.Socket subscriber = context.socket(ZMQ.SUB);
		subscriber.connect("tcp://localhost:5556");

		//  Subscribe to zipcode, default is NYC, 10001
		String filter = (args.length > 0) ? args[0] : "10001 ";
		subscriber.subscribe(filter.getBytes());

		//  Process 100 updates
		int update_nbr;
		long total_temp = 0;
		for (update_nbr = 0; update_nbr < 100; update_nbr++) {
			//  Use trim to remove the tailing '0' character
			String string = new String(subscriber.recv(0)).trim();
			int zipcode, temperature, relhumidity;

			StringTokenizer sscanf = new StringTokenizer(string, " ");
			String t = sscanf.nextToken();
			zipcode = Integer.valueOf(t);

			t = sscanf.nextToken();
			temperature = Integer.valueOf(t);

			t = sscanf.nextToken();
			relhumidity = Integer.valueOf(t);

			total_temp += temperature;

		}
		System.out.println("Average temperature for zipcode '"
				+ filter + "' was " + (int) (total_temp / update_nbr));
	}
}
