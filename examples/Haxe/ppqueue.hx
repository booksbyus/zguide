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
 * Paranoid Pirate Queue
 * 
 * @see http://zguide.zeromq.org/page:all#Robust-Reliable-Queuing-Paranoid-Pirate-Pattern
 * 
 * Author: rsmith (at) rsbatechnology (dot) co (dot) uk
 */

class PPQueue 
{
	private static inline var HEARTBEAT_LIVENESS = 3;
	private static inline var HEARTBEAT_INTERVAL = 1000;	// msecs
	
	private static inline var PPP_READY = String.fromCharCode(1);
	private static inline var PPP_HEARTBEAT = String.fromCharCode(2);
	
	public static function main() {
		Lib.println("** PPQueue (see: http://zguide.zeromq.org/page:all#Robust-Reliable-Queuing-Paranoid-Pirate-Pattern)");
		
		// Prepare our context and sockets
		var context:ZContext = new ZContext();
		var frontend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		var backend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		frontend.bind("tcp://*:5555");		// For clients
		backend.bind("tcp://*:5556");		// For workerQueue
		
		// Queue of available workerQueue
		var workerQueue = new WorkerQueue(HEARTBEAT_LIVENESS, HEARTBEAT_INTERVAL);
		
		// Send out heartbeats at regular intervals
		var heartbeatAt = Date.now().getTime() + HEARTBEAT_INTERVAL;
		
		var poller = new ZMQPoller();
		
		while (true) {
			poller.unregisterAllSockets();
			poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
			// Only poll frontend clients if we have at least one worker to do stuff
			if (workerQueue.size() > 0) {
				poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
			}
			try {
				poller.poll(HEARTBEAT_INTERVAL * 1000);
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted())
					break;
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
			
			// Handle worker activity
			if (poller.pollin(1)) {
				// use worker addressFrame for LRU routing
				var msg = ZMsg.recvMsg(backend);
				if (msg == null)
					break;		// Interrupted
				
				// Any sign of life from worker means it's ready
				var addressFrame = msg.unwrap();
				var identity = addressFrame.toString();
				
				// Validate control message, or return reply to client
				if (msg.size() == 1) {
					var frame = msg.first();
					if (frame.streq(PPP_READY)) {
						workerQueue.delete(identity);
						workerQueue.append(addressFrame,identity);
					} else if (frame.streq(PPP_HEARTBEAT)) {
						workerQueue.refresh(identity);
					} else {
						Lib.println("E: invalid message from worker");
						Lib.println(msg.toString());
					}
					msg.destroy();
				} else {
					msg.send(frontend);
					workerQueue.append(addressFrame, identity);
				}
			}
			
			if (poller.pollin(2)) {
				// Now get next client request, route to next worker
				var msg = ZMsg.recvMsg(frontend);
				if (msg == null) 
					break;		// Interrupted
				var worker = workerQueue.dequeue();
				msg.push(worker.addressFrame.duplicate());
				msg.send(backend);
			}
			
			// Send heartbeats to idle workerQueue if it's time
			if (Date.now().getTime() >= heartbeatAt) {
				for ( w in workerQueue) {
					var msg = new ZMsg();
					msg.add(w.addressFrame.duplicate());		// Add a duplicate of the stored worker addressFrame frame,
														// to prevent w.addressFrame ZFrame object from being destroyed when msg is sent
					msg.addString(PPP_HEARTBEAT);
					msg.send(backend);
				}
				heartbeatAt = Date.now().getTime() + HEARTBEAT_INTERVAL;
			}
			workerQueue.purge();
		}
		// When we're done, clean up properly
		context.destroy();
		
	}
}

typedef WorkerT = {
	addressFrame:ZFrame,
	identity:String,
	expiry:Float	// in msecs since 1 Jan 1970
};

/**
 * Internal class managing a queue of workerQueue
 */
private class WorkerQueue {
	
	// Stores hash of worker heartbeat expiries, keyed by worker identity
	private var queue:List<WorkerT>;
	
	private var heartbeatLiveness:Int;
	private var heartbeatInterval:Int;
	
	/**
	 * Constructor
	 * @param	liveness
	 * @param	interval
	 */
	public function new(liveness:Int, interval:Int) {
		queue = new List<WorkerT>();
		heartbeatLiveness = liveness;
		heartbeatInterval = interval;
	}
	
	// Implement Iterable typedef signature
	public function iterator():Iterator<WorkerT> {
		return queue.iterator();
	}
	
	/**
	 * Insert worker at end of queue, reset expiry
	 * Worker must not already be in queue
	 * @param	identity
	 */
	public function append(addressFrame:ZFrame,identity:String) {
		if (get(identity) != null) 
			Lib.println("E: duplicate worker identity " + identity);
		else
			queue.add({addressFrame:addressFrame, identity:identity, expiry:generateExpiry()});
	} 
	
	/**
	 * Remove worker from queue, if present
	 * @param	identity
	 */
	public function delete(identity:String) {
		var w = get(identity);
		if (w != null) {
			queue.remove(w);
		}
	}
	
	public function refresh(identity:String) {
		var w = get(identity);
		if (w == null) 
			Lib.println("E: worker " + identity + " not ready");
		else
			w.expiry = generateExpiry();
	}
	
	/**
	 * Pop next worker off queue, return WorkerT
	 * @param	identity
	 */
	public function dequeue():WorkerT {
		return queue.pop();
	}
	
	/**
	 * Look for & kill expired workerQueue
	 */
	public function purge() {
		for (w in queue) {
			if (Date.now().getTime() > w.expiry) {
				queue.remove(w);
			}
		}
	}
	
	/**
	 * Return the size of this worker Queue
	 * @return
	 */
	public function size():Int {
		return queue.length;
	}
	
	/**
	 * Returns a WorkerT anon object if exists in the queue, else null
	 * @param	identity
	 * @return
	 */
	private function get(identity:String):WorkerT {
		for (w in queue) {
			if (w.identity == identity)
				return w;
		}
		return null;	// nothing found
	}
	
	private inline function generateExpiry():Float {
		return Date.now().getTime() + heartbeatInterval * heartbeatLiveness;
	}
}
 