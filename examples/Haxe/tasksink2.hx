package ;

import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Parallel Task sink with kil signalling in Haxe
 * Binds PULL request socket to tcp://localhost:5558
 * Collects results from workers via this socket
 * 
 * See: http://zguide.zeromq.org/page:all#Handling-Errors-and-ETERM
 * 
 * Based on http://zguide.zeromq.org/cs:tasksink2
 * 
 * Use with TaskVent.hx and TaskWork2.hx
 */
class TaskSink2
{

	public static function main() {
		var context:ZMQContext = ZMQContext.instance();

		Lib.println("** TaskSink2 (see: http://zguide.zeromq.org/page:all#Handling-Errors-and-ETERM)");
		
		// Socket to receive messages on
		var receiver:ZMQSocket = context.socket(ZMQ_PULL);
		receiver.bind("tcp://127.0.0.1:5558");
		
        // Socket to send control messages to workers
        var controller:ZMQSocket = context.socket(ZMQ_PUB);
        controller.bind("tcp://127.0.0.1:5559");
        
		// Wait for start of batch
		var msgString = StringTools.trim(receiver.recvMsg().toString());
		
		// Start our clock now
		var tStart = Sys.time();
		
		// Process 100 messages
		var task_nbr:Int;
		for (task_nbr in 0 ... 100) {
			msgString = StringTools.trim(receiver.recvMsg().toString());
			if (task_nbr % 10 == 0) {
				Lib.println(":");		// Print a ":" every 10 messages
			} else {
				Lib.print(".");
			}
		}
        
		// Calculate and report duation of batch
		var tEnd = Sys.time();
		Lib.println("Total elapsed time: " + Math.ceil((tEnd - tStart) * 1000) + " msec");
		
        // Send kill signal to workers
        controller.sendMsg(Bytes.ofString("KILL"));
        Sys.sleep(1.0); // Give 0MQ time to deliver
        
        // Shut down
		receiver.close();
        controller.close();
		context.term();
	}
}