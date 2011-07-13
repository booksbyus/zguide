package ;

import haxe.io.Bytes;
import haxe.Stack;
import neko.Lib;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMQException;

/**
 * Signal Handling
 * 
 * Call 
 */
class Interrupt 
{

	public static function main() {
		var context:ZMQContext = ZMQContext.instance();
		var receiver:ZMQSocket = context.socket(ZMQ_REP);
		receiver.bind("tcp://127.0.0.1:5559");
		
		Lib.println("** Interrupt (see: http://zguide.zeromq.org/page:all#Handling-Interrupt-Signals)");
		
		ZMQ.catchSignals();
		
		Lib.println ("\nPress Ctrl+C");

		while (true) {
			// Blocking read, will exit only on an interrupt (Ctrl+C)
			
			try {
			   var msg:Bytes = receiver.recvMsg();
			} catch (e:ZMQException) {
				if (ZMQ.isInterrupted()) {
					trace ("W: interrupt received, killing server ...\n");
					break;
				}
				
				// Handle other errors
				trace("ZMQException #:" + e.errNo + ", str:" + e.str());
				trace (Stack.toString(Stack.exceptionStack()));
			}
		}
		// Close up gracefully
		receiver.close();
		context.term();
		
		
	}
}