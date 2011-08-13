package ;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;

/**
 * Simple Pirate worker
 * Connects REQ socket to tcp://*:5556
 * Implements worker part of LRU queuing
 * @see http://zguide.zeromq.org/page:all#Basic-Reliable-Queuing-Simple-Pirate-Pattern
 */
class SPWorker 
{

	// Signals workers are ready
	private static inline var LRU_READY:String = String.fromCharCode(1);

	public static function main() {
		Lib.println("** SPWorker (see: http://zguide.zeromq.org/page:all#Basic-Reliable-Queuing-Simple-Pirate-Pattern)");
		
		var ctx = new ZContext();
		var worker = ctx.createSocket(ZMQ_REQ);
		
		// Set random identity to make tracing easier
		var identity = ZHelpers.setID(worker);
		worker.connect("tcp://localhost:5556");
		
		// Tell broker we're ready for work
		Lib.println("I: (" + identity + ") worker ready");
		ZFrame.newStringFrame(LRU_READY).send(worker);
		
		var cycles = 0;
		while (true) {
			var msg = ZMsg.recvMsg(worker);
			if (msg == null)
				break;		// Interrupted
			cycles++;
			
			// Simulate various problems, after a few cycles
			if (cycles > 3 && ZHelpers.randof(5) == 0) {
				Lib.println("I: simulating a crash");
				break;
			}
			else if (cycles > 3 && ZHelpers.randof(5) == 0) {
				Lib.println("I: simulating CPU overload");
				Sys.sleep(3.0);
				if (ZMQ.isInterrupted())
					break;
			}
			Lib.println("I: ("+identity+") normal reply");
			Sys.sleep(1.0);			// Do some heavy work
			msg.send(worker);
		}
		ctx.destroy();
		
	}
}