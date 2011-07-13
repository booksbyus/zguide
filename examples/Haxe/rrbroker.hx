package ;
import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQException;

/**
 * Simple request-reply broker
 * 
 * Use with RrClient.hx and RrServer.hx
 */
class RrBroker 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
        
		Lib.println("** RrBroker (see: http://zguide.zeromq.org/page:all#A-Request-Reply-Broker)");
        
        var frontend:ZMQSocket = context.socket(ZMQ_ROUTER);
        var backend:ZMQSocket = context.socket(ZMQ_DEALER);
        frontend.bind("tcp://*:5559");
        backend.bind("tcp://*:5560");
        
        Lib.println("Launch and connect broker.");
        
        // Initialise poll set
        var items:ZMQPoller = context.poller();
        items.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
        items.registerSocket(backend, ZMQ.ZMQ_POLLIN());
        
        var more = false;
        var msgBytes:Bytes;
        
        ZMQ.catchSignals();
        
        while (true) {
            try {
                items.poll();
                if (items.pollin(1)) {
                    while (true) {
                        // receive message
                        msgBytes = frontend.recvMsg();
                        more = frontend.hasReceiveMore();
                        // broker it to backend
                        backend.sendMsg(msgBytes, { if (more) SNDMORE else null; } );
                        if (!more) break;
                    }
                }
                
                if (items.pollin(2)) {
                    while (true) {
                        // receive message
                        msgBytes = backend.recvMsg();
                        more = backend.hasReceiveMore();
                        // broker it to frontend
                        frontend.sendMsg(msgBytes, { if (more) SNDMORE else null; } );
                        if (!more) break;
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
        frontend.close();
        backend.close();
        context.term();
    }
}