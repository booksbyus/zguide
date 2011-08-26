package ;

import ZHelpers;

import neko.Lib;
import neko.Sys;
import haxe.io.Bytes;
import org.zeromq.ZMQ;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQSocket;

/**
 * Demonstrate identities as used by the request-reply pattern.  Run this
 * program by itself.
 */

class Identity 
{

	public static function main() {
        var context:ZContext = new ZContext();
        Lib.println("** Identity (see: http://zguide.zeromq.org/page:all#Request-Reply-Envelopes)");
        
        // Socket facing clients
        var sink:ZMQSocket = context.createSocket(ZMQ_ROUTER);
        sink.bind("inproc://example");
		
		// First allow 0MQ to set the identity
		var anonymous:ZMQSocket = context.createSocket(ZMQ_REQ);
		anonymous.connect("inproc://example");
		anonymous.sendMsg(Bytes.ofString("ROUTER uses a generated UUID"));
		ZHelpers.dump(sink);
		
		// Then set the identity ourself
		var identified:ZMQSocket = context.createSocket(ZMQ_REQ);
		identified.setsockopt(ZMQ_IDENTITY, Bytes.ofString("Hello"));
		identified.connect("inproc://example");
		identified.sendMsg(Bytes.ofString("ROUTER socket uses REQ's socket identity"));
		ZHelpers.dump(sink);
		
		context.destroy();
		
	}
	
}