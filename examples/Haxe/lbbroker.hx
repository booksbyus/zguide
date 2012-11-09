package ;
import haxe.io.Bytes;
import neko.Lib;
#if (neko || cpp)
import neko.vm.Thread;
#end
import haxe.Stack;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQException;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;

/**
 * Least - recently used (LRU) queue device
 * Clients and workers are shown here in-process
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * NB: LRUQueue deliberately uses the lower-level ZMQxxx.hx classes.
 * See LRUQueue2 for a cleaner implementation using the Zxxx.hx classes, modelled on czmq
 * 
 * See: http://zguide.zeromq.org/page:all#A-Request-Reply-Message-Broker
 */

class LRUQueue 
{

	private static inline var NBR_CLIENTS = 10;
	private static inline var NBR_WORKERS = 3;
	
	/**
	 * Basic request-reply client using REQ socket.
	 */
	public static function clientTask() {
		var context:ZContext = new ZContext();
		var client:ZMQSocket = context.createSocket(ZMQ_REQ);
		var id = ZHelpers.setID(client);
		client.connect("ipc:///tmp/frontend.ipc");
		
		// Send request, receive reply
		client.sendMsg(Bytes.ofString("HELLO"));
		var reply = client.recvMsg();
		Lib.println("Client "+id+": " + reply.toString());
		
		context.destroy();
	}
	
	/**
	 * Worker using REQ socket to do LRU routing.
	 */
	public static function workerTask() {
		var context:ZContext = new ZContext();
		var worker:ZMQSocket = context.createSocket(ZMQ_REQ);
		var id = ZHelpers.setID(worker);
		worker.connect("ipc:///tmp/backend.ipc");
		
		// Tell broker we're ready to do work
		worker.sendMsg(Bytes.ofString("READY"));
		
		while (true) {
			// Read and save all frames until we get an empty frame
			// In this example, there is only 1 but it could be more.
			var address = worker.recvMsg();
			var empty = worker.recvMsg();
			
			// Get request, send reply
			var request = worker.recvMsg();
			Lib.println("Worker "+id+": " + request.toString());
			
			worker.sendMsg(address, SNDMORE);
			worker.sendMsg(empty, SNDMORE);
			worker.sendMsg(Bytes.ofString("OK"));
		}
		
		context.destroy();
	}
	
	public static function main() {
		Lib.println("** LRUQueue (see: http://zguide.zeromq.org/page:all#A-Request-Reply-Message-Broker)");
		var client_nbr:Int = 0, worker_nbr:Int;
		
#if php
		// PHP appears to require tasks to be forked before main process creates ZMQ context
		for (client_nbr in 0 ... NBR_CLIENTS) {
			forkClientTask();
		}
		for (worker_nbr in 0 ... NBR_WORKERS) {
			forkWorkerTask();
		}
#end
		// Prepare our context and sockets
		var context:ZContext = new ZContext();
		var frontend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		var backend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		frontend.bind("ipc:///tmp/frontend.ipc");
		backend.bind("ipc:///tmp/backend.ipc");
#if !php
		// Non-PHP targets require threads to be created after main thread has set up ZMQ Context
		for (client_nbr in 0 ... NBR_CLIENTS) {
			Thread.create(clientTask);
		}
		for (worker_nbr in 0 ... NBR_WORKERS) {
			Thread.create(workerTask);
		}
#end		

		// Logic of LRU loop:
		// - Poll backend always, frontend only if 1 or more worker si ready
		// - If worker replies, queue worker as ready and forward reply
		//   to client if necessary.
		// - If client requests, pop next worker and send request to it.
		
		// Queue of available workers
		var workerQueue:List<String> = new List<String>();
		
		var poller:ZMQPoller = new ZMQPoller();
		poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
		
		client_nbr = NBR_CLIENTS;
		while (true) {
			poller.unregisterSocket(frontend);
			if (workerQueue.length > 0) {
				// Only poll frontend if there is at least 1 worker ready to do work
				poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
			}
			
			try {
				poller.poll( -1 );
			} catch (e:ZMQException) {
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
			// Handle worker activity on backend
			if (poller.pollin(1)) {
				// Queue worker address for LRU routing
				var workerAddr = backend.recvMsg();
				if (workerQueue.length < NBR_WORKERS) 
					workerQueue.add(workerAddr.toString());
				
				// Second frame is empty
				var empty = backend.recvMsg();
				
				// Third frame is READY or else a client reply address
				var clientAddr = backend.recvMsg();
				
				// If client reply, send rest back to frontend
				if (clientAddr.toString() != "READY") {
					empty = backend.recvMsg();
					var reply = backend.recvMsg();
					frontend.sendMsg(clientAddr, SNDMORE);
					frontend.sendMsg(Bytes.ofString(""), SNDMORE);
					frontend.sendMsg(reply);
					if (--client_nbr == 0)
						break;		// Exit after NBR_CLIENTS messages
				}
			}
			
			if (poller.pollin(2)) {
				// Now get next client request, route to LRU worker
				// Client request is [address][empty][request]
				var clientAddr = frontend.recvMsg();
				var empty = frontend.recvMsg();
				var request = frontend.recvMsg();
				
				backend.sendMsg(Bytes.ofString(workerQueue.pop()), SNDMORE);
				backend.sendMsg(Bytes.ofString(""), SNDMORE);
				backend.sendMsg(clientAddr, SNDMORE);
				backend.sendMsg(Bytes.ofString(""), SNDMORE);
				backend.sendMsg(request);
				
			}
		}
		
		context.destroy();
	}
	
#if php
		private static inline function forkWorkerTask() {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					LRUQueue::workerTask();
					exit();
				}');
			return;
		}

		private static inline function forkClientTask() {
				untyped __php__('
					$pid = pcntl_fork();
					if ($pid == 0) {
						LRUQueue::clientTask();
						exit();
					}');
				return;
			}
#end
	
}