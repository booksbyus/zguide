package ;

import haxe.Stack;
import neko.Lib;
import neko.Sys;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQException;

import MDP;
import MDCliAPI;

/**
 * Titanic client example
 * Implements client side of http://rfc.zeromq.org/spec:9
 * 
 * Wraps MDCltAPI
 * 
 * @author Richard J Smith
 */

class TIClient 
{

	public static function main()
	{
		
		Lib.println("** TIClient (see: http://zguide.zeromq.org/page:all#Disconnected-Reliability-Titanic-Pattern)");
		
		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var session = new TICltAPI("tcp://localhost:5555", verbose);
		
		// 1. Send 'echo' request to Titanic
		var request = ZMsg.newStringMsg("Hello World");
		var reply = session.request("echo", request);
		var uuid:ZFrame = null;
		if (reply != null) {
			uuid = reply.pop();
			Lib.println("I: request UUID:" + uuid.toString());
			
			// 2. Wait until we get a reply
			while (!ZMQ.isInterrupted()) {
				Sys.sleep(0.1);
				reply = session.reply(uuid);
				if (reply != null) {
					var replyString = reply.last().toString();
					Lib.println("I: Reply: " + replyString);
					
					// 3. Close request
					reply = session.close(uuid);
					break;
				} else {
					Lib.println("I: no reply yet, trying again...");
					Sys.sleep(5.0);		// Try again in 5 seconds
				}
			}
		}
		session.destroy();
	}
}

/**
 * Titanic Protocol Client API class (synchronous)
 */
private class TICltAPI {
	// Private instance fields 
	
	/** Synchronous Majordomo Client API object */
	private var clientAPI:MDCliAPI;
	
	/** Connection string to reach broker */
	private var broker:String;
	
	/** Print activity to stdout */
	private var verbose:Bool;
	
	/** Logger function used in verbose mode */
	private var log:Dynamic->Void;
	
	/**
	 * Constructor
	 * @param	broker
	 * @param	verbose
	 */
	public function new(broker:String, ?verbose:Bool=false, ?logger:Dynamic->Void) {
		this.broker = broker;
		this.verbose = verbose;
		this.clientAPI = new MDCliAPI(broker, verbose, logger);
		if (logger != null) 
			log = logger;
		else
			log = Lib.println;
			
		clientAPI.connectToBroker();
	}
		
	/**
	 * Destructor
	 */
	public function destroy() {
		clientAPI.destroy();
	}
	
	/**
	 * Sends a TI request message to a designated MD service, via a TI proxy
	 * Takes ownership of request message and destroys it when sent.
	 * @param	service
	 * @param	request
	 * @return
	 */
	public function request(service:String, request:ZMsg):ZMsg {
		request.pushString(service);
		var ret = callService("titanic.request", request);
		return {
			if (ret.OK)
				ret.msg;
			else
				null;
		};
	}
	
	/**
	 * Sends a TI Reply message to a TI server
	 * @param	uuid	UUID of original TI Request message, as returned from previous sendRequest method call
	 * @return
	 */
	public function reply(uuid:ZFrame):ZMsg {
		var request = new ZMsg();
		request.add(uuid.duplicate());
		var ret = callService("titanic.reply", request);
		return {
			if (ret.OK)
				ret.msg;
			else
				null;
		};
	}
	
	/**
	 * Sends a TI Close message to a TI server
	 * @param	uuid	UUID of original TI Request message, as returned from previous sendRequest method call
	 * @return
	 */
	public function close(uuid:ZFrame):ZMsg {
		var request = new ZMsg();
		request.add(uuid.duplicate());
		var ret = callService("titanic.close", request);
		return {
			if (ret.OK)
				ret.msg;
			else
				null;
		};
	}
	
	/**
	 * Calls TSP service
	 * Returns response if successful (status code 200 OK), else null
	 * @param	service
	 * @param	request
	 * @return
	 */
	private function callService(service:String, request:ZMsg): { msg:ZMsg, OK:Bool} {
		var reply = clientAPI.send(service, request);
		if (reply != null) {
			var status = reply.pop();
			if (status.streq("200")) {
				return {msg:reply, OK:true};
			}
			else
			if (status.streq("400")) {
				log("E: client fatal error");
				return {msg:null, OK: false};
			}
			else
			if (status.streq("500")) {
				log("E: server fatal error");
				return {msg:null, OK:false};
			}
		}
		return {msg:null, OK:true};	// Interrupted or failed
	}
	
	
		
}