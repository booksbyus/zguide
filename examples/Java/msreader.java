import org.zeromq.ZMQ;

//
//  Reading from multiple sockets in Java
//  This version uses a simple recv loop
//
//  Nicola Peduzzi <thenikso@gmail.com>
//
public class msreader {

	public static void main(String[] args) throws Exception {
		//  Prepare our context and sockets
		ZMQ.Context context = ZMQ.context(1);

		// Connect to task ventilator
		ZMQ.Socket receiver = context.socket(ZMQ.PULL);
		receiver.connect("tcp://localhost:5557");

		//  Connect to weather server
		ZMQ.Socket subscriber = context.socket(ZMQ.SUB);
		subscriber.connect("tcp://localhost:5556");
		subscriber.subscribe("10001 ".getBytes());

		//  Process messages from both sockets
		//  We prioritize traffic from the task ventilator
		while (true) {
			//  Process any waiting tasks
			byte[] task;
			while((task = receiver.recv(ZMQ.NOBLOCK)) != null) {
					//  process task
			}
			//  Process any waiting weather updates
			byte[] update;
			while ((update = subscriber.recv(ZMQ.NOBLOCK)) != null) {
				//  process weather update
			}
			//  No activity, so sleep for 1 msec
			Thread.sleep(1000000);
		}
	}
}
