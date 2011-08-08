package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZFrame;
import org.zeromq.ZLoop;
import org.zeromq.ZMsg;
#if (neko || cpp)
import neko.vm.Thread;
#end
import haxe.Stack;
import org.zeromq.ZContext;
import org.zeromq.ZSocket;
using org.zeromq.ZSocket;
import org.zeromq.ZMQ;
import org.zeromq.ZMQException;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;

/**
 * Least - recently used (LRU) queue device 3
 * Demonstrates use of Zxxxx.hx API and reactor style using the ZLoop class.
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 *  
 * See: http://zguide.zeromq.org/page:all#A-High-Level-API-for-MQ
 */

class LRUQueue3
{

	private static inline var NBR_CLIENTS = 10;
	private static inline var NBR_WORKERS = 3;
	
	// Signals workers are ready
	private static inline var LRU_READY:String = String.fromCharCode(1);
	
	private static inline var WORKER_DONE:Bytes = Bytes.ofString("OK");
	
	/**
	 * Basic request-reply client using REQ socket.
	 */
	public static function clientTask() {
		var context:ZContext = new ZContext();
		var client:ZMQSocket = context.createSocket(ZMQ_REQ);
		var id = ZHelpers.setID(client);
		client.connectEndpoint("ipc", "/tmp/frontend.ipc");
		
		while (true) {
			ZFrame.newStringFrame("HELLO").send(client);
			var reply = ZFrame.recvFrame(client);
			if (reply == null) {
				break;
			}
			Lib.println("Client "+id+": " + reply.toString());
			Sys.sleep(1);
		}
		
		context.destroy();
	}
	
	/**
	 * Worker using REQ socket to do LRU routing.
	 */
	public static function workerTask() {
		var context:ZContext = new ZContext();
		var worker:ZMQSocket = context.createSocket(ZMQ_REQ);
		var id = ZHelpers.setID(worker);
		worker.connectEndpoint("ipc", "/tmp/backend.ipc");
		
		// Tell broker we're ready to do work
		ZFrame.newStringFrame(LRU_READY).send(worker);
		
		// Process messages as they arrive
		while (true) {
			var msg:ZMsg = ZMsg.recvMsg(worker);

			if (msg == null) {
				break;
			}
			// Lib.println("Worker " + id + " received " + msg.toString());
			msg.last().reset(WORKER_DONE);
			msg.send(worker);
		}
		context.destroy();
	}
	
	// Hold information baout our LRU Queue structure
	private static var frontend:ZMQSocket;
	private static var backend:ZMQSocket;
	private static var workerQueue:List<ZFrame>;
	
	/**
	 * Handle input from client, on frontend
	 * @param	loop
	 * @param	socket
	 * @return
	 */
	private static function handleFrontEnd(loop:ZLoop, socket:ZMQSocket):Int {
		var msg = ZMsg.recvMsg(frontend);
		if (msg != null) {
			msg.wrap(workerQueue.pop());
			msg.send(backend);
			// Cancel reader on frontend if we went from 1 to 0 workers
			if (workerQueue.length == 0)
				loop.unregisterPoller({socket:frontend,event:ZMQ.ZMQ_POLLIN()});
		}
		return 0;
	}
	
	/**
	 * Hande input from worker on backend
	 * @param	loop
	 * @param	socket
	 * @return
	 */
	private static function handleBackEnd(loop:ZLoop, socket:ZMQSocket):Int {
		var msg:ZMsg = ZMsg.recvMsg(backend);
		if (msg != null) {
			var address = msg.unwrap();
			workerQueue.add(address);
			if (workerQueue.length == 1)
				loop.registerPoller( { socket:frontend, event:ZMQ.ZMQ_POLLIN() }, handleFrontEnd);
			// Forward message to client if it is not a READY
			var frame = msg.first();
			if (frame.streq(LRU_READY))
				msg.destroy();
			else
				msg.send(frontend);
		}
		return 0;
	}
	
	public static function main() {
		Lib.println("** LRUQueue3 (see: http://zguide.zeromq.org/page:all#A-High-Level-API-for-MQ)");
		
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
		frontend = context.createSocket(ZMQ_ROUTER);
		backend = context.createSocket(ZMQ_ROUTER);
		frontend.bindEndpoint("ipc", "/tmp/frontend.ipc");
		backend.bindEndpoint("ipc", "/tmp/backend.ipc");
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
		
		// Initialise queue of available workers
		workerQueue = new List<ZFrame>();
		
		// Prepare reactor and fire it up
		var reactor:ZLoop = new ZLoop();
		reactor.registerPoller( { socket:backend, event:ZMQ.ZMQ_POLLIN() }, handleBackEnd);
		reactor.start();
		reactor.destroy();
		
		// When we're done, clean up properly
		for (f in workerQueue) {
			f.destroy();
		}
		context.destroy();
	}
	
#if php
		private static inline function forkWorkerTask() {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					LRUQueue3::workerTask();
					exit();
				}');
			return;
		}

		private static inline function forkClientTask() {
				untyped __php__('
					$pid = pcntl_fork();
					if ($pid == 0) {
						LRUQueue3::clientTask();
						exit();
					}');
				return;
			}
#end
	
}