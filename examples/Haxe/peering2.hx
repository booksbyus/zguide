package ;
import org.zeromq.ZMQException;
import ZHelpers;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import neko.io.File;
import neko.io.FileInput;
#if (neko || cpp)
import neko.vm.Thread;
#end
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZFrame;

/**
 * Broker peering simulation (part 2)
 * Prototypes the request-reply flow
 * 
 * While this example runs in a single process (for cpp & neko) and forked processes (for php), that is just
 * to make it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * See: http://zguide.zeromq.org/page:all#Prototyping-the-Local-and-Cloud-Flows
 * 
 * NB: If running from Run.hx, set ARG_OFFSET to 1
 * If running directly, set ARG_OFFSET to 0
 */
class Peering2 
{

	private static inline var NBR_CLIENTS = 10;
	private static inline var NBR_WORKERS = 3;
	private static inline var LRU_READY:String = String.fromCharCode(1);	// Signals workers are ready
	private static inline var WORKER_DONE = "OK";
	
	// Our own name; in practise this would be configured per node
	private static var self:String;
	
	private static inline var ARG_OFFSET = 1;
	
	/**
	 * Request - reply client using REQ socket
	 */
	private static function clientTask() {
		var ctx = new ZContext();
		var client = ctx.createSocket(ZMQ_REQ);
		client.connect("ipc:///tmp/" + self + "-localfe.ipc");
		
		while (true) {
			ZFrame.newStringFrame("HELLO").send(client);
			var reply = ZFrame.recvFrame(client);
			if (reply == null) {
				break;
			}
			Lib.println("Client: " + reply.toString());
			Sys.sleep(1);
		}
		ctx.destroy();
	}
	
	/**
	 * Worker using REQ socket to do LRU routing
	 */
	public static function workerTask() {
		var context:ZContext = new ZContext();
		var worker:ZMQSocket = context.createSocket(ZMQ_REQ);
		worker.connect("ipc:///tmp/"+self+"-localbe.ipc");
		
		// Tell broker we're ready to do work
		ZFrame.newStringFrame(LRU_READY).send(worker);
		
		// Process messages as they arrive
		while (true) {
			var msg:ZMsg = ZMsg.recvMsg(worker);
			if (msg == null) {
				break;
			}
			Lib.println("Worker received " + msg.last().toString());
			msg.last().reset(Bytes.ofString(WORKER_DONE));
			msg.send(worker);
		}
		context.destroy();
	}
	
	public static function main() {
		Lib.println("** Peering2 (see: http://zguide.zeromq.org/page:all#Prototyping-the-Local-and-Cloud-Flows)");
		
		// First argument is this broker's name
		// Other arguments are our peers' names
		if (Sys.args().length < 2+ARG_OFFSET) {
			Lib.println("syntax: ./Peering2 me {you} ...");
			return;
		}
	
		self = Sys.args()[0 + ARG_OFFSET];
		
#if php	
		// Start local workers
		for (worker_nbr in 0 ... NBR_WORKERS) {
			forkWorkerTask();
		}
		
		// Start local clients
		for (client_nbr in 0 ... NBR_CLIENTS) {
			forkClientTask();
		}
#end
		
		Lib.println("I: preparing broker at " + self + " ...");
		
		// Prepare our context and sockets
		var ctx = new ZContext();
		var endpoint:String;
		
		// Bind cloud frontend to endpoint
		var cloudfe = ctx.createSocket(ZMQ_ROUTER);
		cloudfe.setsockopt(ZMQ_IDENTITY, Bytes.ofString(self));
		cloudfe.bind("ipc:///tmp/" + self + "-cloud.ipc");
		
		// Connect cloud backend to all peers
		var cloudbe = ctx.createSocket(ZMQ_ROUTER);
		cloudbe.setsockopt(ZMQ_IDENTITY, Bytes.ofString(self));
		for (argn in 1 + ARG_OFFSET ... Sys.args().length) {
			var peer = Sys.args()[argn];
			Lib.println("I: connecting to cloud frontend at '" + peer + "'");
			cloudbe.connect("ipc:///tmp/" + peer + "-cloud.ipc");
		}
		// Prepare local frontend and backend
		var localfe = ctx.createSocket(ZMQ_ROUTER);
		localfe.bind("ipc:///tmp/" + self + "-localfe.ipc");
		var localbe = ctx.createSocket(ZMQ_ROUTER);
		localbe.bind("ipc:///tmp/" + self + "-localbe.ipc");
		
		// Get user to tell us when we can start...
		Lib.println("Press Enter when all brokers are started: ");
		var f:FileInput = File.stdin();
		var str:String = f.readLine();
		
#if !php	
		// Start local workers
		for (worker_nbr in 0 ... NBR_WORKERS) {
			Thread.create(workerTask);
		}
		
		// Start local clients
		for (client_nbr in 0 ... NBR_CLIENTS) {
			Thread.create(clientTask);
		}
#end

		//  Interesting part
		//  -------------------------------------------------------------
		//  Request-reply flow
		//  - Poll backends and process local/cloud replies
		//  - While worker available, route localfe to local or cloud

		//  Queue of available workers
		var capacity = 0;
		var workerQueue:List<ZFrame> = new List<ZFrame>();
		var backend = new ZMQPoller();
		backend.registerSocket(localbe, ZMQ.ZMQ_POLLIN());
		backend.registerSocket(cloudbe, ZMQ.ZMQ_POLLIN());
		var frontend = new ZMQPoller();
		frontend.registerSocket(localfe, ZMQ.ZMQ_POLLIN());
		frontend.registerSocket(cloudfe, ZMQ.ZMQ_POLLIN());
		
		while (true) {
			var ret = 0;
			
			try {
				// If we have no workers anyhow, wait indefinitely
				ret = backend.poll( {
					if (capacity > 0) 1000 * 1000 else -1; } );
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					break;
				}
				trace (e.toString());
				return;
			}
			
			var msg:ZMsg = null;
	
			// Handle reply from local worker
			if (backend.pollin(1)) {
				msg = ZMsg.recvMsg(localbe);
				if (msg == null)
					break;		// Interrupted
				var address = msg.unwrap();
				workerQueue.add(address);
				capacity++;
				
				// If it's READY, don't route the message any further
				var frame = msg.first();
				if (frame.streq(LRU_READY))
					msg.destroy();	
			}
			// Or handle reply from peer broker
			else if (backend.pollin(2)) {
				msg = ZMsg.recvMsg(cloudbe);
				if (msg == null)
					break;
				// We don't use peer broker address for anything
				var address = msg.unwrap();
			}
			// Route reply to cloud if it's addressed to a broker
			if (msg != null && !msg.isEmpty()) {
				for (argv in 1 + ARG_OFFSET ... Sys.args().length) {
					if (!msg.isEmpty() && msg.first().streq(Sys.args()[argv])) {
						msg.send(cloudfe);
					}
				}
			}
			// Route reply to client if we still need to
			if (msg != null && !msg.isEmpty()) {
				msg.send(localfe);
			}
			// Now route as many client requests as we can handle
			while (capacity > 0) {
				try {
					ret = frontend.poll(0);
				} catch (e:ZMQException) {
					if (ZMQ.isInterrupted())
						break;
					trace (e.toString());	
					return;
				}
				var reroutable = 0;
				// We'll do peer brokers first, to prevent starvation
				if (frontend.pollin(2)) {
					msg = ZMsg.recvMsg(cloudfe);
					reroutable = 0;
				} else if (frontend.pollin(1)){
					msg = ZMsg.recvMsg(localfe);
					reroutable = 1;
				} else
					break;			// No work, go back to the backends
				
				// If reroutable, send to cloud 20% of the time
				// Here we'd normally use cloud status information
				//
				if (reroutable > 0 && Sys.args().length > 1 + ARG_OFFSET && ZHelpers.randof(5) == 0) {
					// Route to random broker peer
					var randomPeer = ZHelpers.randof(Sys.args().length - (2 + ARG_OFFSET)) + (1 + ARG_OFFSET);
					trace ("Routing to peer#"+randomPeer+":" + Sys.args()[randomPeer]);
					msg.wrap(ZFrame.newStringFrame(Sys.args()[randomPeer]));
					msg.send(cloudbe);
				} else {
					msg.wrap(workerQueue.pop());
					msg.send(localbe);
					capacity--;
				}
			}
		}
		// When we're done, clean up properly
		ctx.destroy();	
		
	}
	
#if php
	private static inline function forkClientTask() {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					Peering2::clientTask();
					exit();
				}');
			return;
	}

	private static inline function forkWorkerTask() {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					Peering2::workerTask();
					exit();
				}');
			return;
	}

#end
}