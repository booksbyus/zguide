package ;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;

/**
 * Lazy Pirate server
 * Binds REP socket to tcp://*:5555
 * Like HWServer except:
 *  - echoes request as-is
 *  - randomly runs slowly, or exists to simulate a crash.
 * 
 * @see http://zguide.zeromq.org/page:all#Client-side-Reliability-Lazy-Pirate-Pattern
 * 
 */
class LPServer 
{

	public static function main() {
		Lib.println("** LPServer (see: http://zguide.zeromq.org/page:all#Client-side-Reliability-Lazy-Pirate-Pattern)");
		var ctx = new ZContext();
		var server = ctx.createSocket(ZMQ_REP);
		server.bind("tcp://*:5555");
		
		var cycles = 0;
		while (true) {
			var requestFrame = ZFrame.recvFrame(server);
			cycles++;
			
			// Simulate various problems, after a few cycles
			if (cycles > 3 && ZHelpers.randof(3) == 0) {
				Lib.println("I: simulating a crash");
				break;
			}
			else if (cycles > 3 && ZHelpers.randof(3) == 0) {
				Lib.println("I: simulating CPU overload");
				Sys.sleep(2.0);
			}
			Lib.println("I: normal request (" + requestFrame.toString() + ")");
			Sys.sleep(1.0);			// Do some heavy work
			requestFrame.send(server);
			requestFrame.destroy();
			
		}
		server.close();
		ctx.destroy();
	}
}