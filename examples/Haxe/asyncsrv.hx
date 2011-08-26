package ;
import neko.Lib;
import org.zeromq.ZMQException;
#if !php
import neko.Random;
import neko.vm.Thread;
#end
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

/**
 * Asynchronous client-server (DEALER to ROUTER)
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 *  
 * See: http://zguide.zeromq.org/page:all#Asynchronous-Client-Server
 */
class ASyncSrv 
{

#if php
	private static inline var internalServerEndpoint:String = "ipc:///tmp/backend";
#else
	private static inline var internalServerEndpoint:String = "inproc://backend";
#end
	/**
	 * This is our client task
	 * It connects to the server, and then sends a request once per second
	 * It collects responses as they arrive, and it prints them out.  We will
	 * run several client tasks in parallel, each with a different random ID.
	 */
	public static function clientTask(context:ZContext) {
		var client:ZMQSocket = context.createSocket(ZMQ_DEALER);
		
		// Set random identity to make tracing easier
		var id = ZHelpers.setID(client);
		client.connect("tcp://localhost:5570");
		
		//trace ("Started client " + id);
		
		var poller = new ZMQPoller();
		poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
		var request_nbr = 0;
		
		while (true) {
			for (centitick in 0 ... 100) {
				try {
					poller.poll(10000);	// Poll for 10ms
				} catch (e:ZMQException) {
					if (ZMQ.isInterrupted())
						break;
					trace (e.toString());
					break;
				}
				if (poller.pollin(1)) {
					var msg:ZMsg = ZMsg.recvMsg(client);
					Lib.println("Client: " + id + " received:" + msg.last().toString());
					msg.destroy();
				}
			}
			if (poller == null)
				break;		// Interrupted
			ZMsg.newStringMsg("request #" + ++request_nbr).send(client);
		}
		context.destroy();
	}
	
	/**
	 * Accept a request and reply with the same text a random number of
	 * times, with random delays between replies.
	 */
	public static function serverWorker(context:ZContext) {
		var worker:ZMQSocket = context.createSocket(ZMQ_DEALER);
		worker.connect(internalServerEndpoint);
		
		while (true) {
			// The DEALER socket gives us the address envelope and message
			var msg = ZMsg.recvMsg(worker);
			var address:ZFrame = msg.pop();
			var content:ZFrame = msg.pop();
			//trace ("Got request from " + address.toString());
			if (content == null) 
				break;
			msg.destroy();
			
			// Send 0...4 replies back
#if php
			var replies = untyped __php__('rand(0, 4)');
#else			
			var replies = new Random().int(4);
#end			
			for (reply in 0...replies) {
				// Sleep for some fraction of a second
#if php
				Sys.sleep((untyped __php__('rand(0, 1000)') + 1) / 1000);
#else				
				Sys.sleep(new Random().float() + 0.001);
#end				
				address.send(worker, ZFrame.ZFRAME_MORE + ZFrame.ZFRAME_REUSE);
				content.send(worker, ZFrame.ZFRAME_REUSE);
			}
			address.destroy();
			content.destroy();
		}
	}

	/**
	 * This is our server task
	 * It uses the multithreaded server model to deal requests out to a pool
	 * of workers and route replies back to clients.  One worker can handle
	 * one request at a time but one client can talk to multiple workers at
	 * once.
	 */
	public static function serverTask(context:ZContext) {		

#if php
		for (thread_nbr in 0 ... 5) {
			forkServerWorker(context);
		}
#end		
		// Frontend socket talks to clients over TCP
		var frontend = context.createSocket(ZMQ_ROUTER);
	    frontend.bind("tcp://*:5570");
		
		// Backend socket talks to workers over inproc
		var backend = context.createSocket(ZMQ_DEALER);
		backend.bind(internalServerEndpoint);
		
		// Launch pool of worker threads, precise number is not critical
#if !php		
		for (thread_nbr in 0 ... 5) {
			Thread.create(callback(serverWorker,context));
		}
#end		
		// Connect backend to frontend via queue device
		// We could do this via 
		//		new ZMQDevice(ZMQ_QUEUE, frontend, backend);
		// but doing it ourselves means we can debug this more easily
	
		// Switch messages between frontend and backend
		var poller:ZMQPoller = new ZMQPoller();
		poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
		poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
		
		while (true) {
			try {
				poller.poll( -1);
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted())
					break;
				trace (e.toString());
				break;
			}
			if (poller.pollin(1)) {
				var msg = ZMsg.recvMsg(frontend);
				//trace("Request from client:"+msg.toString());
				msg.send(backend);
			}
			if (poller.pollin(2)) {
				var msg = ZMsg.recvMsg(backend);
				//trace ("Reply from worker:" + msg.toString());
				msg.send(frontend);
			}
		}
		context.destroy();
	}
	
	public static function main() {
		Lib.println("** ASyncSrv (see: http://zguide.zeromq.org/page:all#Asynchronous-Client-Server)");

		var context = new ZContext();	
			
#if php
		forkClientTask(context);
		forkClientTask(context);
		forkClientTask(context);
		forkServerTask(context);
#else		
		Thread.create(callback(clientTask, context));
		Thread.create(callback(clientTask, context));
		Thread.create(callback(clientTask, context));
		Thread.create(callback(serverTask, context));
#end
		
		// Run for 5 seconds then quit
		Sys.sleep(5);
		context.destroy();
	}
	
#if php
	private static inline function forkServerWorker(context:ZContext) {
		untyped __php__('
			$pid = pcntl_fork();
			if ($pid == 0) {
				ASyncSrv::serverWorker($context);
				exit();
			}');
		return;
	}

	private static inline function forkClientTask(context:ZContext) {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					ASyncSrv::clientTask($context);
					exit();
				}');
			return;
		}

	private static inline function forkServerTask(context:ZContext) {
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					ASyncSrv::serverTask($context);
					exit();
				}');
			return;
		}
		
#end
	
}