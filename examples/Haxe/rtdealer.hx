package ;

import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
#if (neko || cpp)
import neko.Random;
import neko.vm.Thread;
#end
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.ZMQSocket;

/**
 * Custom routing Router to Dealer
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example.  Each thread has its own
 * context and conceptually acts as a separate process.
 * 
 * See: http://zguide.zeromq.org/page:all#Router-to-Dealer-Routing
 */
class RTDealer 
{

	public static function workerTask(id:String) {
		var context:ZContext = new ZContext();
		var worker:ZMQSocket = context.createSocket(ZMQ_DEALER);
		worker.setsockopt(ZMQ_IDENTITY, Bytes.ofString(id));
		worker.connect("ipc:///tmp/routing.ipc");
		
		var total = 0;
		while (true) {
			// We receive one part, with the workload
			var request:ZFrame = ZFrame.recvFrame(worker);
			if (request == null) break;
			if (request.streq("END")) {
				Lib.println(id + " received: " + total);
				break;
			}
			total++;
		}
		context.destroy();
	}
	
	public static function main() {

		Lib.println("** RTDealer (see: http://zguide.zeromq.org/page:all#Router-to-Dealer-Routing)");

		// Implementation note: Had to move php forking before main thread ZMQ Context creation to
		// get the main thread to receive messages from the child processes.
#if php
		// For PHP, use processes, not threads
		forkWorkerTasks();
#else
		var workerA = Thread.create(callback(workerTask, "A"));
		var workerB = Thread.create(callback(workerTask, "B"));
#end		

		var context:ZContext = new ZContext();
		var client:ZMQSocket = context.createSocket(ZMQ_ROUTER);
		// Implementation note: Had to add the /tmp prefix to get this to work on Linux Ubuntu 10
		client.bind("ipc:///tmp/routing.ipc");
		
		// Wait for threads to connect, since otherwise the messages
		// we send won't be routable.
		Sys.sleep(1);
		
		// Send 10 tasks scattered to A twice as often as B
		var workload = ZFrame.newStringFrame("This is the workload");
		var address:ZFrame;
#if !php
		var rnd = new Random();
		rnd.setSeed(Date.now().getSeconds());
#end
		for (task_nbr in 0 ... 10) {
			// Send two message parts, first the address...
			var randNumber:Int;
#if php
			randNumber = untyped __php__('rand(0, 2)');
#else		
			randNumber = rnd.int(2);
#end
			if (randNumber > 0)
				address = ZFrame.newStringFrame("A");
			else
				address = ZFrame.newStringFrame("B");
			
			address.send(client, ZFrame.ZFRAME_MORE);	
			
			// And then the workload
			workload.send(client, ZFrame.ZFRAME_REUSE);
		}
		
		ZFrame.newStringFrame("A").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("END").send(client);
		
		ZFrame.newStringFrame("B").send(client, ZFrame.ZFRAME_MORE);
		ZFrame.newStringFrame("END").send(client);
		
		workload.destroy();
		context.destroy();
		
	}

#if php
	private static inline function forkWorkerTasks() {
		untyped __php__('
			$pid = pcntl_fork();
			if ($pid == 0) {
				RTDealer::workerTask("A");
				exit();
			}');
			untyped __php__('
				$pid = pcntl_fork();
				if ($pid == 0) {
					RTDealer::workerTask("B");
					exit();
				}');
		return;
	}
#end

}