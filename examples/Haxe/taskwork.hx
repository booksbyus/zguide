package ;

import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Task worker in Haxe
 * Connects PULL socket to tcp://localhost:5557
 * Collects workloads from ventilator via that socket
 * Connects PUSH socket to tcp://localhost:5558
 * Sends results to sink via that socket
 * 
 * See: http://zguide.zeromq.org/page:all#Divide-and-Conquer
 * 
 * Based on code from: http://zguide.zeromq.org/java:taskwork
 * 
 * Use with TaskVent.hx and TaskSink.hx
 */
class TaskWork 
{

	public static function main() {
		var context:ZMQContext = ZMQContext.instance();

		Lib.println("** TaskWork (see: http://zguide.zeromq.org/page:all#Divide-and-Conquer)");
		
		// Socket to receive messages on
		var receiver:ZMQSocket = context.socket(ZMQ_PULL);
		receiver.connect("tcp://127.0.0.1:5557");

		// Socket to send messages to
		var sender:ZMQSocket = context.socket(ZMQ_PUSH);
		sender.connect("tcp://127.0.0.1:5558");
		
		// Process tasks forever
		while (true) {
			var msgString = StringTools.trim(receiver.recvMsg().toString());
			var sec:Float = Std.parseFloat(msgString) / 1000.0;
			Lib.print(msgString + ".");
			
			// Do the work
			Sys.sleep(sec);
			
			// Send results to sink
			sender.sendMsg(Bytes.ofString(""));
		}
		
		
	}
}