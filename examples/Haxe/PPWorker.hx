package ;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQException;
import org.zeromq.ZMsg;
import org.zeromq.ZSocket;

/**
 * Paranoid Pirate worker
 * 
 * @see http://zguide.zeromq.org/page:all#Robust-Reliable-Queuing-Paranoid-Pirate-Pattern
 * 
 * Author: rsmith (at) rsbatechnology (dot) co (dot) uk
 */
class PPWorker 
{
	private static inline var HEARTBEAT_LIVENESS = 3;
	private static inline var HEARTBEAT_INTERVAL = 1000;	// msecs
	private static inline var INTERVAL_INIT = 1000;			// Initial reconnect
	private static inline var INTERVAL_MAX = 32000;			// After exponential backoff
	
	private static inline var PPP_READY = String.fromCharCode(1);
	private static inline var PPP_HEARTBEAT = String.fromCharCode(2);

	/**
	 * Helper function that returns a new configured socket
	 * connected to the Paranid Pirate queue
	 * @param	ctx
	 * @return
	 */
	private static function workerSocket(ctx:ZContext):ZMQSocket {
		var worker = ctx.createSocket(ZMQ_DEALER);
		worker.connect("tcp://localhost:5556");
		
		// Tell queue we're ready for work
		Lib.println("I: worker ready");
		ZFrame.newStringFrame(PPP_READY).send(worker);
		
		return worker;
	}
	
	public static function main() {
		Lib.println("** PPWorker (see: http://zguide.zeromq.org/page:all#Robust-Reliable-Queuing-Paranoid-Pirate-Pattern)");
		
		var ctx = new ZContext();
		var worker = workerSocket(ctx);
		
		// If liveness hits zero, queue is considered disconnected
		var liveness = HEARTBEAT_LIVENESS;
		var interval = INTERVAL_INIT;
		
		// Send out heartbeats at regular intervals
		var heartbeatAt = Date.now().getTime() + HEARTBEAT_INTERVAL;
		
		var cycles = 0;
		var poller = new ZMQPoller();
		poller.registerSocket(worker, ZMQ.ZMQ_POLLIN());
		
		while (true) {
			try {
				poller.poll(HEARTBEAT_INTERVAL * 1000);
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted())
					break;
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
			if (poller.pollin(1)) {
				// Get message
				//  - 3-part envelope + content -> request
				//  - 1-part HEARTBEAT -> heartbeat
				var msg = ZMsg.recvMsg(worker);
				if (msg == null)
					break;		// Interrupted
				if (msg.size() == 3) {
					// Simulate various problems, after a few cycles
					cycles++;
					if (cycles > 3 && ZHelpers.randof(5) == 0) {
						Lib.println("I: simulating a crash");
						msg.destroy();
						break;
					} else if (cycles > 3 && ZHelpers.randof(5) == 0) {
						Lib.println("I: simulating CPU overload");
						Sys.sleep(3.0);
						if (ZMQ.isInterrupted())
							break;
					}
					Lib.println("I: normal reply");
					msg.send(worker);
					liveness = HEARTBEAT_LIVENESS;
					Sys.sleep(1.0);		// Do some heavy work
					if (ZMQ.isInterrupted())
						break;
				} else if (msg.size() == 1) {
					var frame = msg.first();
					if (frame.streq(PPP_HEARTBEAT))
						liveness = HEARTBEAT_LIVENESS;
					else {
						Lib.println("E: invalid message");
						Lib.println(msg.toString());
					}
					msg.destroy();
				} else {
					Lib.println("E: invalid message");
					Lib.println(msg.toString());
				}
				interval = INTERVAL_INIT;
			} else if (--liveness == 0) {
				Lib.println("W: heartbeat failure, can't reach queue");
				Lib.println("W: reconnecting in " + interval + " msec...");
				Sys.sleep(interval / 1000.0);
				
				if (interval < INTERVAL_MAX)
					interval *= 2;
				ctx.destroySocket(worker);
				worker = workerSocket(ctx);
				poller.unregisterAllSockets();
				poller.registerSocket(worker, ZMQ.ZMQ_POLLIN());
				
				liveness = HEARTBEAT_LIVENESS;
			}
			
			// Send heartbeat to queue if it's time
			if (Date.now().getTime() > heartbeatAt) {
				heartbeatAt = Date.now().getTime() + HEARTBEAT_INTERVAL;
				Lib.println("I: worker heartbeat");
				ZFrame.newStringFrame(PPP_HEARTBEAT).send(worker);
			}
		}
		ctx.destroy();
	}
}