package ;
import haxe.io.Bytes;
import neko.Lib;
import org.zeromq.ZMQ;
import org.zeromq.ZMQContext;
import org.zeromq.ZMQSocket;

/**
 * Weather update server in Haxe
 * Binds PUB socket to tcp://*:5556
 * Publishes random weather updates
 * 
 * See: http://zguide.zeromq.org/page:all#Getting-the-Message-Out
 * 
 * Use with WUClient.hx
 */
class WUServer 
{

	public static function main() {
		var context:ZMQContext = ZMQContext.instance();
		
		Lib.println("** WUServer (see: http://zguide.zeromq.org/page:all#Getting-the-Message-Out)");

		var publisher:ZMQSocket = context.socket(ZMQ_PUB);
		publisher.bind("tcp://127.0.0.1:5556");
		
		while (true) {
			// Get values that will fool the boss
			var zipcode, temperature, relhumidity;           
			zipcode = Std.random(100000) + 1;
			temperature = Std.random(215) - 80 + 1;
			relhumidity = Std.random(50) + 10 + 1;
			
			// Send message to all subscribers
			var update:String = zipcode + " " + temperature + " " + relhumidity;
			publisher.sendMsg(Bytes.ofString(update));
		}
	}
}