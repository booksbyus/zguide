package ;

import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
#if (neko || cpp)
import neko.vm.Thread;
#end
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.ZMQSocket;

import ZHelpers;

/**
 * Custom routing Router to Mama (ROUTER to REQ)
 * 
 * While this example runs in a single process (for cpp & neko), that is just
 * to make it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * See: http://zguide.zeromq.org/page:all#Least-Recently-Used-Routing-LRU-Pattern
 */
class RTMama 
{

	private static inline var NBR_WORKERS = 10;
	
	public static function workerTask() {
		var context:ZContext = new ZContext();
		var worker:ZMQSocket = context.createSocket(ZMQ_REQ);
		
		// Use a random string identity for ease here
		var id = ZHelpers.setID(worker);
		worker.connect("ipc:///tmp/routing.ipc");
		
		var total = 0;
		while (true) {
			// Tell the router we are ready
			ZFrame.newStringFrame("ready").send(worker);
			// Get workload from router, until finished
			var workload:ZFrame = ZFrame.recvFrame(worker);
			if (workload == null) break;
			if (workload.streq("END")) {
				Lib.println("Processed: " + total + " tasks");
				break;
			}
			total++;
			
			// Do some random work
			Sys.sleep((ZHelpers.randof(1000) + 1) / 1000.0);
		}
		context.destroy();
	}
	
	public static function main() {

		Lib.println("** RTMama (see: http://zguide.zeromq.org/page:all#Least-Recently-Used-Routing-LRU-Pattern)");

		// Implementation note: Had to move php forking before main thread ZMQ Context creation to
		// get the main thread to receive messages from the child processes.
		for (worker_nbr in 0 ... NBR_WORKERS) {
#if php
		forkWorkerTask();
#else
		Thread.create(workerTask);
#end
		}

		var context:ZContext = new ZContext();
		var client:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		// Implementation note: Had to add the /tmp prefix to get this to work on Linux Ubuntu 10
		client.bind("ipc:///tmp/routing.ipc");

		
		Sys.sleep(1);
		for (task_nbr in 0 ... NBR_WORKERS * 10) {
			// LRU worker is next waiting in queue
			var address:ZFrame = ZFrame.recvFrame(client);
			var empty:ZFrame = ZFrame.recvFrame(client);
			var ready:ZFrame = ZFrame.recvFrame(client);
			
			address.send(client, ZFrame.ZFRAME_MORE);
			ZFrame.newStringFrame("").send(client, ZFrame.ZFRAME_MORE);
			ZFrame.newStringFrame("This is the workload").send(client);
		}
		// Now ask mamas to shut down and report their results
		for (worker_nbr in 0 ... NBR_WORKERS) {
			var address:ZFrame = ZFrame.recvFrame(client);
			var empty:ZFrame = ZFrame.recvFrame(client);
			var ready:ZFrame = ZFrame.recvFrame(client);
			
			address.send(client, ZFrame.ZFRAME_MORE);
			ZFrame.newStringFrame("").send(client, ZFrame.ZFRAME_MORE);
			ZFrame.newStringFrame("END").send(client);
		}
		context.destroy();
	}
	
#if php
	private static inline function forkWorkerTask() {
		untyped __php__('
			$pid = pcntl_fork();
			if ($pid == 0) {
				RTMama::workerTask();
				exit();
			}');
		return;
	}
#end
	
}