package ;

import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
#if !php
import neko.vm.Thread;
#end
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQException;

/**
 * Multithreaded Hello World Server
 * 
 * See: http://zguide.zeromq.org/page:all#Multithreading-with-MQ
 * Use with HelloWorldClient.hx
 * 
 */
class MTServer 
{

	static function worker() {
		var context:ZMQContext = ZMQContext.instance();
		
		// Socket to talk to dispatcher
		var responder:ZMQSocket = context.socket(ZMQ_REP);
#if (neko || cpp)        
		responder.connect("inproc://workers");
#elseif php
        responder.connect("ipc://workers.ipc");
#end        
		ZMQ.catchSignals();
		
		while (true) {
			
			try {
				// Wait for next request from client
				var request:Bytes = responder.recvMsg();
				
				trace ("Received request:" + request.toString());
				
				// Do some work
				Sys.sleep(1);
				
				// Send reply back to client
				responder.sendMsg(Bytes.ofString("World"));
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					break;
				}
				trace (e.toString());
			}
		} 
		responder.close();
		return null;
	}
	
	/**
	 * Implements a reqeust/reply QUEUE broker device
	 * Returns if poll is interrupted
	 * @param	ctx
	 * @param	frontend
	 * @param	backend
	 */
	static function queueDevice(ctx:ZMQContext, frontend:ZMQSocket, backend:ZMQSocket) {
		
		// Initialise pollset
		var poller:ZMQPoller = ctx.poller();
		poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
		poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
		
		ZMQ.catchSignals();
		
		while (true) {
			try {
				poller.poll();
				if (poller.pollin(1)) {
					var more:Bool = true;
					while (more) {
						// Receive message
						var msg = frontend.recvMsg();
						more = frontend.hasReceiveMore();
						
						// Broker it
						backend.sendMsg(msg, { if (more) SNDMORE else null; } );
					}
				}
				
				if (poller.pollin(2)) {
					var more:Bool = true;
					while (more) {
						// Receive message
						var msg = backend.recvMsg();
						more = backend.hasReceiveMore();
						
						// Broker it
						frontend.sendMsg(msg, { if (more) SNDMORE else null; } );
					}
				}
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					break;
				}
				// Handle other errors
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
				
			}

		}
		
	}
	public static function main() {		
		var context:ZMQContext = ZMQContext.instance();
		
		Lib.println ("** MTServer (see: http://zguide.zeromq.org/page:all#Multithreading-with-MQ)");
		
		// Socket to talk to clients
		var clients:ZMQSocket = context.socket(ZMQ_ROUTER);
		clients.bind ("tcp://*:5556");
		
		// Socket to talk to workers
		var workers:ZMQSocket = context.socket(ZMQ_DEALER);
		
#if (neko || cpp)        
		workers.bind ("inproc://workers");
        
		// Launch worker thread pool
		var workerThreads:List<Thread> = new List<Thread>();
		for (thread_nbr in 0 ... 5) {
			workerThreads.add(Thread.create(worker));
		}
#elseif php
		workers.bind ("ipc://workers.ipc");

        // Launch pool of worker processes, due to php's lack of thread support
        // See: https://github.com/imatix/zguide/blob/master/examples/PHP/mtserver.php
        for (thread_nbr in 0 ... 5) {
            untyped __php__('
                $pid = pcntl_fork();
                if ($pid == 0) {
                    // Running in child process
                    worker();
                    exit();
                }');
        }
#end        
		// Invoke request / reply broker (aka QUEUE device) to connect clients to workers
		queueDevice(context, clients, workers);
		
		// Close up shop
		clients.close();
		workers.close();
		context.term();
	}
}