
import org.zeromq.ZMQ;

/**
 *
 * @author Marcus McCurdy <marcus.mccurdy@gmail.com>
 */
public class taskwork2 {

	public static void main(String[] args) throws InterruptedException {
		ZMQ.Context context = ZMQ.context(1);

		ZMQ.Socket receiver = context.socket(ZMQ.PULL);
		receiver.connect("tcp://localhost:5557");

		ZMQ.Socket sender = context.socket(ZMQ.PUSH);
		sender.connect("tcp://localhost:5558");

		ZMQ.Socket controller = context.socket(ZMQ.SUB);
		controller.connect("tcp://localhost:5559");
		controller.subscribe("".getBytes());

		ZMQ.Poller items = context.poller(2);
		items.register(receiver, ZMQ.Poller.POLLIN);
		items.register(controller, ZMQ.Poller.POLLIN);

		while (true) {
			
			items.poll();
			
			if (items.pollin(0)) {
				byte[] message = receiver.recv(0);
				
				String string = new String(message).trim();
				long nsec = Long.parseLong(string);
				
				//  Simple progress indicator for the viewer
				System.out.flush();
				System.out.print(string + '.');

				//  Do the work
				Thread.sleep(nsec);

				//  Send results to sink
				sender.send("".getBytes(), 0);

				// Simple progres indicator for the viewer
				System.out.println(".");
			}
			if (items.pollin(1)) {
				break; // Exit loop
			}

		}
		
		// Finished
		receiver.close();
		sender.close();
		controller.close();
		context.term();
		System.exit(0);
	}
}
