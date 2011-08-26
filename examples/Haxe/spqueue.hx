package ;
import haxe.Stack;
import neko.Lib;
import org.zeromq.ZFrame;
import org.zeromq.ZContext;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;
import org.zeromq.ZMQException;

/**
 * Simple Pirate queue
 * This is identical to the LRU pattern, with no reliability mechanisms
 * at all.  It depends on the client for recovery.  Runs forever.
 * 
 * @see http://zguide.zeromq.org/page:all#Basic-Reliable-Queuing-Simple-Pirate-Pattern
 */

class SPQueue 
{

	// Signals workers are ready
	private static inline var LRU_READY:String = String.fromCharCode(1);
	
	public static function main() {
		Lib.println("** SPQueue (see: http://zguide.zeromq.org/page:all#Basic-Reliable-Queuing-Simple-Pirate-Pattern)");
		
		// Prepare our context and sockets
		var context:ZContext = new ZContext();
		var frontend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		var backend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		frontend.bind("tcp://*:5555");		// For clients
		backend.bind("tcp://*:5556");		// For workers
			
		// Queue of available workers
		var workerQueue:List<ZFrame> = new List<ZFrame>();
		
		var poller:ZMQPoller = new ZMQPoller();
		poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
			
		while (true) {
			poller.unregisterSocket(frontend);
			if (workerQueue.length > 0) {
				// Only poll frontend if there is at least 1 worker ready to do work
				poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
			}
			
			try {
				poller.poll( -1 );
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted())
					break;
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
			if (poller.pollin(1)) {
				// Use worker address for LRU routing
				var msg = ZMsg.recvMsg(backend);
				if (msg == null) 
					break;		// Interrupted
				var address = msg.unwrap();
				workerQueue.add(address);
				
				// Forward message to client if it's not a READY
				var frame = msg.first();
				if (frame.streq(LRU_READY)) 
					msg.destroy();
				else
					msg.send(frontend);	
			}
			if (poller.pollin(2)) {
				// Get client request, route to first available worker
				var msg = ZMsg.recvMsg(frontend);
				if (msg != null) {
					msg.wrap(workerQueue.pop());
					msg.send(backend);
				}
			}
		}
		// When we're done, clean up properly
		for (f in workerQueue) {
			f.destroy();
		}
		context.destroy();
	}
}