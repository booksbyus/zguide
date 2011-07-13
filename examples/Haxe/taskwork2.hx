package ;

import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;

/**
 * Parallel Task worker with kill signalling in Haxe
 * Connects PULL socket to tcp://localhost:5557
 * Collects workloads from ventilator via that socket
 * Connects PUSH socket to tcp://localhost:5558
 * Sends results to sink via that socket
 * 
 * See: http://zguide.zeromq.org/page:all#Handling-Errors-and-ETERM
 * 
 * Based on code from: http://zguide.zeromq.org/java:taskwork2
 */
class TaskWork2
{

	public static function main() {
		var context:ZMQContext = ZMQContext.instance();

		Lib.println("** TaskWork2 (see: http://zguide.zeromq.org/page:all#Handling-Errors-and-ETERM)");
		
		// Socket to receive messages on
		var receiver:ZMQSocket = context.socket(ZMQ_PULL);
		receiver.connect("tcp://127.0.0.1:5557");

		// Socket to send messages to
		var sender:ZMQSocket = context.socket(ZMQ_PUSH);
		sender.connect("tcp://127.0.0.1:5558");
		
        // Socket to receive controller messages from
        var controller:ZMQSocket = context.socket(ZMQ_SUB);
        controller.connect("tcp://127.0.0.1:5559");
        controller.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
        
        var items:ZMQPoller = context.poller();
        items.registerSocket(receiver, ZMQ.ZMQ_POLLIN());
        items.registerSocket(controller, ZMQ.ZMQ_POLLIN());
        
        var msgString:String;
        
		// Process tasks forever
		while (true) {
            var numSocks = items.poll();
            if (items.pollin(1)) {
                // receiver socket has events
                msgString = StringTools.trim(receiver.recvMsg().toString());
                var sec:Float = Std.parseFloat(msgString) / 1000.0;
                Lib.print(msgString + ".");
                
                // Do the work
                Sys.sleep(sec);
                
                // Send results to sink
                sender.sendMsg(Bytes.ofString(""));    
            }
			if (items.pollin(2)) {
                break; // Exit loop
            }
		}
        receiver.close();
        sender.close();
        controller.close();
        context.term();
	}
}