package ;

import neko.Lib;
import haxe.io.Bytes;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Hello World Client
 * Connects REQ socket to tcp://localhost:5559
 * Sends "Hello" to server, expects "World" back
 * 
 * See: http://zguide.zeromq.org/page:all#A-Request-Reply-Broker
 * 
 * Use with RrServer and RrBroker
 */
class RrClient 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
		Lib.println("** RrClient (see: http://zguide.zeromq.org/page:all#A-Request-Reply-Broker)");

		var requester:ZMQSocket = context.socket(ZMQ_REQ);
		requester.connect ("tcp://localhost:5559");
		
        Lib.println ("Launch and connect client.");
        
		// Do 10 requests, waiting each time for a response
		for (i in 0...10) {
			var requestString = "Hello ";
			// Send the message
			requester.sendMsg(Bytes.ofString(requestString));
			
			// Wait for the reply
			var msg:Bytes = requester.recvMsg();
			
			Lib.println("Received reply " + i + ": [" + msg.toString() + "]");
			
		}
		
		// Shut down socket and context
		requester.close();
		context.term();
    }
}