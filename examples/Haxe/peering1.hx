package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMsg;
import org.zeromq.ZSocket;

/**
 * Broker peering simulation (part 1)
 * Prototypes the state flow.
 * 
 * NB: If running from Run.hx, set ARG_OFFSET to 1
 * If running directly, set ARG_OFFSET to 0
 */
class Peering1 
{
	private static inline var ARG_OFFSET = 1;
	
	public static function main() {
		Lib.println("** Peering1 (see: http://zguide.zeromq.org/page:all#Prototyping-the-State-Flow)");
		
		// First argument is this broker's name
		// Other arguments are our peers' names
		if (Sys.args().length < 2+ARG_OFFSET) {
			Lib.println("syntax: ./Peering1 me {you} ...");
			return;
		}
		
		var self = Sys.args()[0+ARG_OFFSET];
		Lib.println("I: preparing broker at " + self + " ...");

		// Prepare our context and sockets
		var ctx = new ZContext();
		var statebe = ctx.createSocket(ZMQ_PUB);
		statebe.bind("ipc:///tmp/" + self + "-state.ipc");
		
		// Connect statefe to all peers
		var statefe = ctx.createSocket(ZMQ_SUB);
		statefe.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
		for (argn in 1+ARG_OFFSET ... Sys.args().length) {
			var peer = Sys.args()[argn];
			Lib.println("I: connecting to state backend at '" + peer + "'");
			statefe.connect("ipc:///tmp/" + peer + "-state.ipc");
		}
		// Send out status messages to peers, and collect from peers
		// The ZMQPoller timeout defines our own heartbeating
		//
		var poller = new ZMQPoller();
		while (true) {
			// Initialise poll set
			poller.registerSocket(statefe, ZMQ.ZMQ_POLLIN());
			try {
				// Poll for activity, or 1 second timeout
				var res = poller.poll(1000 * 1000);
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted())
					break;
				trace (e.toString());
				return;
			}
			// Handle incoming status messages
			if (poller.pollin(1)) {
				var msg = ZMsg.recvMsg(statefe);
				var peerNameFrame = msg.first();
				var availableFrame = msg.last(); 
				Lib.println(peerNameFrame.toString() + " - " + availableFrame.toString() + " workers free");
			} else {
				// Send random value for worker availability
				// We stick our own address onto the envelope
				var msg:ZMsg = new ZMsg();
				msg.addString(self);
				msg.addString(Std.string(ZHelpers.randof(10)));
				msg.send(statebe);
			}
		}
		
		ctx.destroy();
	}
}