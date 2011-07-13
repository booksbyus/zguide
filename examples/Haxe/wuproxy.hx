package ;

import haxe.io.Bytes;
import haxe.Stack;

import neko.Lib;

import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQException;

/**
 * Weather proxy device.
 * 
 * See: http://zguide.zeromq.org/page:all#A-Publish-Subscribe-Proxy-Server
 * 
 * Use with WUClient and WUServer
 */
class WUProxy 
{

    public static function main() {
        var context:ZMQContext = ZMQContext.instance();
		Lib.println("** WUProxy (see: http://zguide.zeromq.org/page:all#A-Publish-Subscribe-Proxy-Server)");
        
        // This is where the weather service sits
        var frontend:ZMQSocket = context.socket(ZMQ_SUB);
        frontend.connect("tcp://localhost:5556");
        
        // This is our public endpoint for subscribers
        var backend:ZMQSocket = context.socket(ZMQ_PUB);
        backend.bind("tcp://10.1.1.0:8100");
        
        // Subscribe on everything
        frontend.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
        
        var more = false;
        var msgBytes:Bytes;
        
        ZMQ.catchSignals();
        
        var stopped = false;
        while (!stopped) {
            try {
                msgBytes = frontend.recvMsg();
                more = frontend.hasReceiveMore();
                
                // proxy it
                backend.sendMsg(msgBytes, { if (more) SNDMORE else null; } );
                if (!more) {
                    stopped = true;
                }
 			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					stopped = true;
				} else {
                    // Handle other errors
                    trace("ZMQException #:" + e.errNo + ", str:" + e.str());
                    trace (Stack.toString(Stack.exceptionStack()));
                }
			}
       }
       frontend.close();
       backend.close();
       context.term();
    }
}