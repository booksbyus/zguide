package ;
import org.zeromq.ZMQException;
import ZHelpers;
import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
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
 * Broker peering simulation (part 3)
 * Prototypes the full flow of status and tasks
 * 
 * While this example runs in a single process (for cpp & neko) and forked processes (for php), that is just
 * to make it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * See: http://zguide.zeromq.org/page:all#Putting-it-All-Together
 * 
 * NB: If running from Run.hx, set ARG_OFFSET to 1
 * If running directly, set ARG_OFFSET to 0
 */
class Peering3 
{

	private static inline var NBR_CLIENTS = 10;
	private static inline var NBR_WORKERS = 3;
	private static inline var LRU_READY:String = String.fromCharCode(1);	// Signals workers are ready
	
	// Our own name; in practise this would be configured per node
	private static var self:String;
	
	private static inline var ARG_OFFSET = 1;
	
	/**
	 * Request - reply client using REQ socket
	 * To simulate load, clients issue a burst of requests and then
	 * sleep for a random period.
	 */
	private static function clientTask() {
		var ctx = new ZContext();
		var client = ctx.createSocket(ZMQ_REQ);
		client.connect("ipc:///tmp/" + self + "-localfe.ipc");
		var monitor = ctx.createSocket(ZMQ_PUSH);
		monitor.connect("ipc:///tmp/" + self + "-monitor.ipc");
		
		var poller = new ZMQPoller();
		poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
		
		while (true) {
			Sys.sleep(ZHelpers.randof(5));
			var burst = ZHelpers.randof(14);
			for (i in 0 ... burst) {
				var taskID = StringTools.hex(ZHelpers.randof(0x10000), 4);
				// Send request with random hex ID
				Lib.println("Client send task " + taskID);
				try {
					ZFrame.newStringFrame(taskID).send(client);
				} catch (e:ZMQException) {
					trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
					return;		// quit
				} catch (e:Dynamic) {
					trace (e);
				}
				
				// Wait max ten seconds for a reply, then complain
				try {
					poller.poll(10 * 1000 * 1000);
				} catch (e:ZMQException) {
					if (ZMQ.isInterrupted())
						break;
					trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
					return;		// quit
				}
				if (poller.pollin(1)) {
					var reply = ZFrame.recvFrame(client);
					if (reply == null)
						break;
					// Worker is supposed to answer us with our task id
					if (!reply.streq(taskID)) {
						Lib.println("E: Returned task ID:" + reply.toString() + " does not match requested taskID:" + taskID);
						break;
					}
				} else {
					ZMsg.newStringMsg("E: CLIENT EXIT - lost task " + taskID).send(monitor);
				}
			}
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
			try {
				var msg:ZMsg = ZMsg.recvMsg(worker);
				if (msg == null) {
					context.destroy();
					return;
				}
				Lib.println("Worker received " + msg.last().toString());
				// Workers are busy for 0 / 1/ 2 seconds
				Sys.sleep(ZHelpers.randof(2));
				msg.send(worker);
			} catch (e:ZMQException) {
				trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));				
			}
		}
		context.destroy();
	}
	
	public static function main() {
		Lib.println("** Peering3 (see: http://zguide.zeromq.org/page:all#Putting-it-All-Together)");
		
		// First argument is this broker's name
		// Other arguments are our peers' names
		if (Sys.args().length < 2+ARG_OFFSET) {
			Lib.println("syntax: ./Peering3 me {you} ...");
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
		
		// Bind state backend / publisher to endpoint
		var statebe = ctx.createSocket(ZMQ_PUB);
		statebe.bind("ipc:///tmp/" + self + "-state.ipc");
		
		// Connect cloud backend to all peers
		var cloudbe = ctx.createSocket(ZMQ_ROUTER);
		cloudbe.setsockopt(ZMQ_IDENTITY, Bytes.ofString(self));
		for (argn in 1 + ARG_OFFSET ... Sys.args().length) {
			var peer = Sys.args()[argn];
			Lib.println("I: connecting to cloud frontend at '" + peer + "'");
			cloudbe.connect("ipc:///tmp/" + peer + "-cloud.ipc");
		}
		
		// Connect statefe to all peers
		var statefe = ctx.createSocket(ZMQ_SUB);
		statefe.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
		for (argn in 1+ARG_OFFSET ... Sys.args().length) {
			var peer = Sys.args()[argn];
			Lib.println("I: connecting to state backend at '" + peer + "'");
			statefe.connect("ipc:///tmp/" + peer + "-state.ipc");
		}
		// Prepare local frontend and backend
		var localfe = ctx.createSocket(ZMQ_ROUTER);
		localfe.bind("ipc:///tmp/" + self + "-localfe.ipc");
		var localbe = ctx.createSocket(ZMQ_ROUTER);
		localbe.bind("ipc:///tmp/" + self + "-localbe.ipc");
		
		// Prepare monitor socket
		var monitor = ctx.createSocket(ZMQ_PULL);
		monitor.bind("ipc:///tmp/" + self + "-monitor.ipc");
				
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
		//  Publish-subscribe flow
		//  - Poll statefe and process capacity updates
		//  - Each time capacity changes, broadcast new value
		//  Request-reply flow
		//  - Poll primary and process local/cloud replies
		//  - While worker available, route localfe to local or cloud

		//  Queue of available workers
		var localCapacity = 0;
		var cloudCapacity = 0;
		var workerQueue:List<ZFrame> = new List<ZFrame>();
		
		var primary = new ZMQPoller();
		primary.registerSocket(localbe, ZMQ.ZMQ_POLLIN());
		primary.registerSocket(cloudbe, ZMQ.ZMQ_POLLIN());
		primary.registerSocket(statefe, ZMQ.ZMQ_POLLIN());
		primary.registerSocket(monitor, ZMQ.ZMQ_POLLIN());
		
		
		while (true) {
			trace ("**Start main loop iteration");
			var ret = 0;
			
			try {
				// If we have no workers anyhow, wait indefinitely
				ret = primary.poll( {
					if (localCapacity > 0) 1000 * 1000 else -1; } );
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					break;
				}
				trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
				return;
			}
			
			// Track if capacity changes in this iteration
			var previous = localCapacity;
			
			var msg:ZMsg = null;
	
			// Handle reply from local worker
			if (primary.pollin(1)) {
				msg = ZMsg.recvMsg(localbe);
				if (msg == null)
					break;		// Interrupted
				var address = msg.unwrap();
				workerQueue.add(address);
				localCapacity++;
				
				// If it's READY, don't route the message any further
				var frame = msg.first();
				if (frame.streq(LRU_READY))
					msg.destroy();	
			}
			// Or handle reply from peer broker
			else if (primary.pollin(2)) {
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
						trace ("Route reply to peer:" + Sys.args()[argv]);
						msg.send(cloudfe);
					}
				}
			}
			// Route reply to client if we still need to
			if (msg != null && !msg.isEmpty()) {
				msg.send(localfe);
			}
			
			// Handle capacity updates
			if (primary.pollin(3)) {
				try {
					var msg = ZMsg.recvMsg(statefe);
					trace ("State msg received:" + msg.toString());
					var availableFrame = msg.last(); 
					cloudCapacity = Std.parseInt(availableFrame.data.toString());
				} catch (e:ZMQException) {
					trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));				
				} catch (e:Dynamic) {
					trace (e);
				}
			}
			// Handle monitor message
			if (primary.pollin(4)) {
				try {
					var status = ZMsg.recvMsg(monitor);
					Lib.println(status.first().data.toString());
					return;
				} catch (e:ZMQException) {
					trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));				
				} catch (e:Dynamic) {
					trace (e);
				}
			}
			
			trace ("** Polling secondary sockets");
			//  Now route as many clients requests as we can handle
			//  - If we have local capacity we poll both localfe and cloudfe
			//  - If we have cloud capacity only, we poll just localfe
			//  - Route any request locally if we can, else to cloud
			//
			while (localCapacity + cloudCapacity > 0) {
				trace ("  ** polling secondary, with total capacity:" + Std.string(localCapacity + cloudCapacity));
				var secondary = new ZMQPoller();
				secondary.registerSocket(localfe, ZMQ.ZMQ_POLLIN());
				
				if (localCapacity > 0) {
					secondary.registerSocket(cloudfe, ZMQ.ZMQ_POLLIN());
				}
				try {
					ret = secondary.poll(0);
				} catch (e:ZMQException) {
					if (ZMQ.isInterrupted())
						break;
					trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
					return;
				}
				// We'll do peer brokers first, to prevent starvation
				trace (" ** Secondary poll completed");
				if (secondary.pollin(1)) {
					trace (" ** About to receive from localfe");
					msg = ZMsg.recvMsg(localfe);
					trace (msg.toString());
				} else if (secondary.pollin(2)) {
					trace ("  ** About to receive from cloudfe");
					msg = ZMsg.recvMsg(cloudfe);
					trace (msg.toString());
				} else {
					trace (" ** No requests, go back to primary");
					break;			// No work, go back to the primary
				}
				if (localCapacity > 0) {
					var frame = workerQueue.pop();
					msg.wrap(frame);
					msg.send(localbe);
					localCapacity--;
				} else {
					// Route to random broker peer
					var randomPeer = ZHelpers.randof(Sys.args().length - (2 + ARG_OFFSET)) + (1 + ARG_OFFSET);
					trace ("Routing to peer#"+randomPeer+":" + Sys.args()[randomPeer]);
					msg.wrap(ZFrame.newStringFrame(Sys.args()[randomPeer]));
					msg.send(cloudbe);
				} 
			}
			
			trace ("Updating status :"+ Std.string(localCapacity != previous));
			if (localCapacity != previous) {
				// We stick our own address onto the envelope
				msg = new ZMsg();
				msg.add(ZFrame.newStringFrame(Std.string(localCapacity)));
				msg.wrap(ZFrame.newStringFrame(self));
				trace ("Updating status:" + msg.toString());
				msg.send(statebe);
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