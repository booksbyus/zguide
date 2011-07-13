package ;

import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import neko.io.File;
import neko.io.FileInput;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQException;
import org.zeromq.ZMQSocket;

/**
 * Task ventilator in Haxe
 * Binds PUSH socket to tcp://localhost:5557
 * Sends batch of tasks to workers via that socket.
 * 
 * Based on code from: http://zguide.zeromq.org/java:taskvent
 * 
 * Use with TaskWork.hx and TaskSink.hx
 */
class TaskVent 
{

	public static function main() {
		
		try {
			var context:ZMQContext = ZMQContext.instance();
			var sender:ZMQSocket = context.socket(ZMQ_PUSH);

			Lib.println("** TaskVent (see: http://zguide.zeromq.org/page:all#Divide-and-Conquer)");
			
			sender.bind("tcp://127.0.0.1:5557");
			
			Lib.println("Press Enter when the workers are ready: ");
			var f:FileInput = File.stdin();
			var str:String = f.readLine();
			Lib.println("Sending tasks to workers ...\n");
			
			// The first message is "0" and signals starts of batch
			sender.sendMsg(Bytes.ofString("0"));
			
			// Send 100 tasks
			var totalMsec:Int = 0;		// Total expected cost in msec
			for (task_nbr in 0 ... 100) {
				var workload = Std.random(100) + 1;	// Generates 1 to 100 msecs
				totalMsec += workload;
				Lib.print(workload + ".");
				sender.sendMsg(Bytes.ofString(Std.string(workload)));
			}
			Lib.println("Total expected cost: " + totalMsec + " msec");
			
			// Give 0MQ time to deliver
			Sys.sleep(1);

			sender.close();
			context.term();
		} catch (e:ZMQException) {
			trace("ZMQException #:" + ZMQ.errNoToErrorType(e.errNo) + ", str:" + e.str());
			trace (Stack.toString(Stack.exceptionStack()));
				
		}
	}
}