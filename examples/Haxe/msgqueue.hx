package ;

import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQDevice;
import org.zeromq.ZContext;
import neko.Lib;

/**
 * Simple message queuing broker
 * Same as request-reply broker but using QUEUE device
 * See: http://zguide.zeromq.org/page:all#Built-in-Devices
 * 
 * Use with RrClient and RrServer
 */
class MsgQueue 
{

	public static function main() {
        var context:ZContext = new ZContext();
        Lib.println("** MsgQueue (see: http://zguide.zeromq.org/page:all#Built-in-Devices)");
        
        // Socket facing clients
        var frontend:ZMQSocket = context.createSocket(ZMQ_ROUTER);
        frontend.bind("tcp://*:5559");
        
        // Socket facing services
        var backend:ZMQSocket = context.createSocket(ZMQ_DEALER);
        backend.bind("tcp://*:5560");
		
		// Start build-in device
		var device = new ZMQDevice(ZMQ_QUEUE, frontend, backend);
		
		// We never get here
		context.destroy();
		
	}
}