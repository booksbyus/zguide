package ;

import haxe.Stack;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZMQException;
import org.zeromq.ZThread;

/**
 * Round-trip demonstrator
 * 
 * While this example runs in a single process, that is just to make
 * it easier to start and stop the example.  Client thread signals to
 * main when it's ready.
 * 
 * @author Richard J Smith
 */

class Tripping 
{
	
	private static function clientTask(ctx:ZContext, pipe:ZMQSocket, ?args:Dynamic) {
		var hello = Bytes.ofString("hello");
		var client = ctx.createSocket(ZMQ_DEALER);
		client.setsockopt(ZMQ_IDENTITY, Bytes.ofString("C"));
		client.connect("tcp://localhost:5555");
		
		Lib.println("Setting up test...");
		Sys.sleep(0.1);		// 100 msecs
		
		var start:Float = 0.0;
		
		Lib.println("Synchronous round-trip test...");
		start = Date.now().getTime();
		for (requests in 0 ... 10000) {
			client.sendMsg(hello);
			var reply = client.recvMsg();
		}
		Lib.println(" " + Std.int((1000.0 * 10000.0) / (Date.now().getTime() - start)) + " calls/second");
		
		Lib.println("Asynchronous round-trip test...");
		start = Date.now().getTime();
		for (requests in 0 ... 10000)
			client.sendMsg(hello);
		for (requests in 0 ... 10000) {
			var reply = client.recvMsg();
		}
		Lib.println(" " + Std.int((1000.0 * 10000.0) / (Date.now().getTime() - start)) + " calls/second");
		
		pipe.sendMsg(Bytes.ofString("done"));
	}
	
	private static function workerTask(?args:Dynamic) {
		var ctx = new ZContext();
		var worker = ctx.createSocket(ZMQ_DEALER);
		worker.setsockopt(ZMQ_IDENTITY, Bytes.ofString("W"));
		worker.connect("tcp://localhost:5556");
		
		while (true) {
			var msg = ZMsg.recvMsg(worker);
			msg.send(worker);
		}
		ctx.destroy();
	}
	
	private static function brokerTask(?args:Dynamic) {
		// Prepare our contexts and sockets
		var ctx = new ZContext();
		var frontend = ctx.createSocket(ZMQ_ROUTER);
		var backend = ctx.createSocket(ZMQ_ROUTER);
		frontend.bind("tcp://*:5555");
		backend.bind("tcp://*:5556");
		
		// Initialise pollset
		var poller = new ZMQPoller();
		poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
		poller.registerSocket(backend, ZMQ.ZMQ_POLLIN());
		
		while (true) {
			try {
				poller.poll(-1);
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					ctx.destroy();
					return;
				}
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
			if (poller.pollin(1)) {
				var msg = ZMsg.recvMsg(frontend);
				var address = msg.pop();
				msg.pushString("W");
				msg.send(backend);
			}
			if (poller.pollin(2)) {
				var msg = ZMsg.recvMsg(backend);
				var address = msg.pop();
				msg.pushString("C");
				msg.send(frontend);
			}
		}
	}
	
	public static function main() {
		Lib.println("** Tripping (see: http://zguide.zeromq.org/page:all#Asynchronous-Majordomo-Pattern)");
		
		// Create threads
		var ctx = new ZContext();
		var client = ZThread.attach(ctx, clientTask, null);
		ZThread.detach(workerTask, null);
		ZThread.detach(brokerTask, null);
		
		// Wait for signal on client pipe
		var signal = client.recvMsg();
		
		ctx.destroy();
	}
}