package ;
import haxe.io.Bytes;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQException;
import org.zeromq.ZMQSocket;

/**
 * Hello World server in Haxe
 * Binds REP to tcp://*:5556
 * Expects "Hello" from client, replies with "World"
 * Use with HelloWorldClient.hx
 * 
 */
class HelloWorldServer 
{

	public static function main() {
		
		var context:ZMQContext = ZMQContext.instance();
		var responder:ZMQSocket = context.socket(ZMQ_REP);

		Lib.println("** HelloWorldServer (see: http://zguide.zeromq.org/page:all#Ask-and-Ye-Shall-Receive)");

		responder.setsockopt(ZMQ_LINGER, 0);
		responder.bind("tcp://*:5556");
		
		try {
			while (true) {
				// Wait for next request from client
				var request:Bytes = responder.recvMsg();
				
				trace ("Received request:" + request.toString());
				
				// Do some work
				Sys.sleep(1);
				
				// Send reply back to client
				responder.sendMsg(Bytes.ofString("World"));
			}
		} catch (e:ZMQException) {
			trace (e.toString());
		}
		responder.close();
		context.term();
		
	}
	
}