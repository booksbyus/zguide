package ;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMsg;

/**
 * Majordomo Protocol worker example
 * Uses the MDWrk API to hde all MDP aspects
 * @author Richard J Smith
 */

class MDWorker 
{

	public static function main() {
		Lib.println("** MDWorker (see: http://zguide.zeromq.org/page:all#Service-Oriented-Reliable-Queuing-Majordomo-Pattern)");

			var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var session = new MDWrkAPI("tcp://localhost:5555", "echo", verbose);
		
		var reply:ZMsg = null;
		while (true) {
			var request = session.recv(reply);
			if (request == null)
				break;		// Interrupted
			reply = request;	
		}
		session.destroy();
	}
	
}