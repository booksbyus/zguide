package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;

import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Hello World client in Haxe.
 * Use with HelloWorldServer.hx and MTServer.hx
 */
class HelloWorldClient 
{
	
	public static function main() {
		var context:ZMQContext = ZMQContext.instance();
		var socket:ZMQSocket = context.socket(ZMQ_REQ);
		
		Lib.println("** HelloWorldClient (see: http://zguide.zeromq.org/page:all#Ask-and-Ye-Shall-Receive)");
		
		trace ("Connecting to hello world server...");
		socket.connect ("tcp://localhost:5556");
		
		// Do 10 requests, waiting each time for a response
		for (i in 0...10) {
			var requestString = "Hello ";
			
			// Send the message
			trace ("Sending request " + i + " ...");
			socket.sendMsg(Bytes.ofString(requestString));
			
			// Wait for the reply
			var msg:Bytes = socket.recvMsg();
			
			trace ("Received reply " + i + ": [" + msg.toString() + "]");
		}
		
		// Shut down socket and context
		socket.close();
		context.term();
	}
}