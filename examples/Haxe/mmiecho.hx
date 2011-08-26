package ;

import neko.Lib;
import neko.Sys;
import org.zeromq.ZMsg;

/**
 * MMI echo query example
 */
class MMIEcho 
{

	public static function main() {
		
		Lib.println("** MMIEcho (see: http://zguide.zeromq.org/page:all#Service-Discovery)");
		
		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var session = new MDCliAPI("tcp://localhost:5555", verbose);
		
		// This is the service we want to look up
		var request = new ZMsg();
		request.addString("echo");
		
		// This is the service we send our request to
		var reply = session.send("mmi.service", request);
		
		if (reply != null) {
			var replyCode = reply.first().toString();
			Lib.println("Lookup echo service: " + replyCode);
		} else
			Lib.println("E: no response from broker, make sure it's running");
		
		session.destroy();	
	}	
}