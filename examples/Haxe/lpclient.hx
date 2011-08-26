package ;
import haxe.Stack;
import neko.Lib;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import org.zeromq.ZMQPoller;
import org.zeromq.ZSocket;

/**
 * Lazy Pirate client
 * Use zmq_poll to do a safe request-reply
 * To run, start lpserver and then randomly kill / restart it.
 * 
 * @see http://zguide.zeromq.org/page:all#Client-side-Reliability-Lazy-Pirate-Pattern
 */

class LPClient 
{

	private static inline var REQUEST_TIMEOUT = 2500;		// msecs, (> 1000!)
	private static inline var REQUEST_RETRIES = 3;			// Before we abandon
	private static inline var SERVER_ENDPOINT = "tcp://localhost:5555";
		
	public static function main() {
		Lib.println("** LPClient (see: http://zguide.zeromq.org/page:all#Client-side-Reliability-Lazy-Pirate-Pattern)");
		var ctx:ZContext = new ZContext();
		Lib.println("I: connecting to server ...");
		var client = ctx.createSocket(ZMQ_REQ);
		if (client == null)
			return;
		client.connect(SERVER_ENDPOINT);
		
		var sequence = 0;
		var retries_left = REQUEST_RETRIES;
				
		var poller = new ZMQPoller();

		while (retries_left > 0 && !ZMQ.isInterrupted()) {
			// We send a request, then we work to get a reply
			var request = Std.string(++sequence);
			ZFrame.newStringFrame(request).send(client);
			
			var expect_reply = true;
			while (expect_reply) {
				poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
				// Poll socket for a reply, with timeout
				try {
					var res = poller.poll(REQUEST_TIMEOUT * 1000);
				} catch (e:ZMQException) {
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
					ctx.destroy();
					return;
				}
				// If we got a reply, process it
				if (poller.pollin(1)) {
					// We got a reply from the server, must match sequence
					var replyFrame = ZFrame.recvFrame(client);
					if (replyFrame == null)
						break;		// Interrupted
					if (Std.parseInt(replyFrame.toString()) == sequence) {
						Lib.println("I: server replied OK (" + sequence + ")");
						retries_left = REQUEST_RETRIES;
						expect_reply = false;
					} else 
						Lib.println("E: malformed reply from server: " + replyFrame.toString());
					replyFrame.destroy();	
				} else if (--retries_left == 0) {
					Lib.println("E: server seems to be offline, abandoning");
					break;
				} else {
					Lib.println("W: no response from server, retrying...");
					// Old socket is confused, close it and open a new one
					ctx.destroySocket(client);
					Lib.println("I: reconnecting to server...");
					client = ctx.createSocket(ZMQ_REQ);
					client.connect(SERVER_ENDPOINT);
					// Send request again, on new socket
					ZFrame.newStringFrame(request).send(client);
				}
				poller.unregisterAllSockets();
			}
		}
		ctx.destroy();
	}
}