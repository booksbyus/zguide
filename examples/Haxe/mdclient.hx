package ;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMsg;

/**
 * Majordomo Protocol client example
 * Uses the MDPCli API to hide all MDP aspects
 * 
 * @author Richard J Smith
 */

class MDClient 
{

	public static function main() {
		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var session = new MDCliAPI("tcp://localhost:5555", verbose);
		var count = 0;
		for (i in 0 ... 100000) {
			var request = new ZMsg();
			request.pushString("Hello world: "+i);
			var reply = session.send("echo", request);
			if (reply != null)
				reply.destroy();
			else
				break;		// Interrupt or failure
			count++;	
		}
		Lib.println(count + " requests/replies processed");
		session.destroy();
	}
}