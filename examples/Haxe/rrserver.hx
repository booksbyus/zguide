package ;

import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQException;
import org.zeromq.ZMQSocket;

/**
 * Hello World server in Haxe
 * Binds REP to tcp://*:5560
 * Expects "Hello" from client, replies with "World"
 * Use with RrClient.hx and RrBroker.hx
 * 
 */
class RrServer 
{

	public static function main() {
		
		var context:ZMQContext = ZMQContext.instance();

		Lib.println("** RrServer (see: http://zguide.zeromq.org/page:all#A-Request-Reply-Broker)");

        // Socket to talk to clients
		var responder:ZMQSocket = context.socket(ZMQ_REP);
		responder.connect("tcp://localhost:5560");
		
        Lib.println("Launch and connect server.");
        
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
				// Handle other errors
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}

        }
		responder.close();
		context.term();
		
	}
	
}