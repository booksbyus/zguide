package ;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMsg;
import org.zeromq.ZMQException;

/**
 * Binary Star Client
 * @author Richard J Smith
 * 
 * @see http://zguide.zeromq.org/page:all#Binary-Star-Implementation
 */

class BStarCli 
{

	private static inline var REQUEST_TIMEOUT = 1000;	// msecs
	private static inline var SETTLE_DELAY = 2000;		// Before failing over
	
	public static function main()
	{
		Lib.println("** BStarCli (see: http://zguide.zeromq.org/page:all#Binary-Star-Implementation)");
		
		var ctx = new ZContext();
		var server = ["tcp://localhost:5001", "tcp://localhost:5002"];
		var server_nbr = 0;
		
		Lib.println("I: connecting to server at " + server[server_nbr]);
		var client = ctx.createSocket(ZMQ_REQ);
		client.connect(server[server_nbr]);
		
		var sequence = 0;
		var poller = new ZMQPoller();
		poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
		while (!ZMQ.isInterrupted()) {
			// We send a request, then we work to get a reply
			var request = Std.string(++sequence);
			ZMsg.newStringMsg(request).send(client);
			
			var expectReply = true;
			while (expectReply) {
				// Poll socket for a reply, with timeout
				try {
					var res = poller.poll(REQUEST_TIMEOUT * 1000);		// Convert timeout to microseconds
				} catch (e:ZMQException) {
					if (!ZMQ.isInterrupted()) {
						trace("ZMQException #:" + e.errNo + ", str:" + e.str());
						trace (Stack.toString(Stack.exceptionStack()));
					} else {
						Lib.println("W: interrupt received, killing client...");	
					}
					ctx.destroy();
					return;
				}
				
				if (poller.pollin(1)) {
					// We got a reply from the server, must match sequence
					var reply = client.recvMsg().toString();
					if (reply != null && Std.parseInt(reply) == sequence) {
						Lib.println("I: server replied OK (" + reply + ")");
						expectReply = false;
						Sys.sleep(1.0);		// One request per second
					} else 
						Lib.println("E: malformed reply from server: " + reply);
				} else {
					Lib.println("W: no response from server, failing over");
					// Old socket is confused; close it and open a new one
					ctx.destroySocket(client);
					server_nbr = (server_nbr + 1) % 2;
					Sys.sleep(SETTLE_DELAY / 1000);
					Lib.println("I: connecting to server at " + server[server_nbr]);
					client = ctx.createSocket(ZMQ_REQ);
					client.connect(server[server_nbr]);
					
					poller.unregisterAllSockets();
					poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
					ZMsg.newStringMsg(request).send(client);
				}
			}
		}
		ctx.destroy();
	}
}