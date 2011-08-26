package ;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZMsg;

/**
 * Majordomo Protocol client example (asynchronous)
 * Uses the MDPCli API to hide all MDP aspects
 * 
 * @author Richard J Smith
 */

class MDClient2
{

	public static function main() {
		Lib.println("** MDClient2 (see: http://zguide.zeromq.org/page:all#Asynchronous-Majordomo-Pattern)");
		
		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var session = new MDCliAPI2("tcp://localhost:5555", verbose);
		for (i in 0 ... 100000) {
			var request = new ZMsg();
			request.pushString("Hello world: "+i);
			session.send("echo", request);
		}
		var count = 0;
		for (i in 0 ... 100000) {
			var reply = session.recv();
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