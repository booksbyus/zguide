import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Context;
import org.zeromq.ZMQ.Socket;

/**
 * Hello World client
 * Connects REQ socket to tcp://localhost:5559
 * Sends "Hello" to server, expects "World" back
 * 
 * Christophe Huntzinger <chuntzin_at_wanadoo.fr>
 */
public class rrclient{
	public static void main (String[] args) {
		Context context = ZMQ.context(1);

		//  Socket to talk to server
		Socket requester = context.socket(ZMQ.REQ);
		requester.connect("tcp://localhost:5559");
		
		System.out.println("launch and connect client.");

		for (int request_nbr = 0; request_nbr < 10; request_nbr++) {
			requester.send("Hello".getBytes(), 0);
			byte[] reply = requester.recv(0);
			String replyValue = new String(reply);
			System.out.println("Received reply "+request_nbr+" ["+replyValue+"]");
		}
		
		//  We never get here but clean up anyhow
		requester.close();
		context.term();
	}
}
