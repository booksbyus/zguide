package ;

/**
 * Binary Star server, using BStar reactor class
 * @author Richard J Smith
 * 
 * See: http://zguide.zeromq.org/page:all#Binary-Star-Reactor
 */

import BStar;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZLoop;
import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;

class BStarSrv2 
{

	/**
	 * Echo service
	 * @param	loop
	 * @param	socket
	 * @return
	 */
	private static function echo(loop:ZLoop, socket:ZMQSocket, args:Dynamic):Int {
		var msg = ZMsg.recvMsg(socket);
		msg.send(socket);
		return 0;
	}
	
	public static function main() {
		Lib.println("** BStarSrv2 (see: http://zguide.zeromq.org/page:all#Binary-Star-Reactor)");

		var bstar:BStar = null;
		
		// Arguments can be either of:
		//  -p	primary server, at tcp://localhost:5001
		//  -b	backup server, at tcp://localhost:5002
		var argArr = Sys.args();
		if (argArr.length > 1 && argArr[argArr.length - 1] == "-p") {
			Lib.println("I: primary master, waiting for backup (slave)");
			bstar = new BStar(true, "tcp://*:5003", "tcp://localhost:5004", true, null);
			bstar.setVoter("tcp://*:5001", ZMQ_ROUTER, echo);
		} else if (argArr.length > 1 && argArr[argArr.length - 1] == "-b") {
			Lib.println("I: backup slave, waiting for primary (master)");
			bstar = new BStar(false, "tcp://*:5004", "tcp://localhost:5003", true, null);
			bstar.setVoter("tcp://*:5002", ZMQ_ROUTER, echo);
		} else {
			Lib.println("Usage: bstartsrv2 { -p | -b }");
			return;
		}
		bstar.start();
		bstar.destroy();
	}
}