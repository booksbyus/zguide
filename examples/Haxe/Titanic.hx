package ;
import haxe.Stack;
import neko.Lib;
import neko.Sys;
import haxe.io.Input;
import neko.FileSystem;
import neko.io.File;
import neko.io.FileInput;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZThread;
import org.zeromq.ZMQException;

/**
 * Titanic service
 * Implements server side of http://rfc.zeromq.org/spec:9
 * @author Richard Smith
 */
class Titanic 
{

	/** Connection string to broker */
	private var broker:String;

	/** Print activity to stdout */
	private var verbose:Bool;
	
	/** Logger function used in verbose mode */
	private var log:Dynamic->Void;
	
	private static inline var UID = "0123456789ABCDEF";
	
	private static inline var TITANIC_DIR = ".titanic";
	
	/**
	 * Main method
	 */
	public static function main() {
		Lib.println("** Titanic (see: http://zguide.zeromq.org/page:all#Disconnected-Reliability-Titanic-Pattern)");

		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		var log = Lib.println;
		
		var ctx = new ZContext();
		
		// Create Titanic worker class
		var titanic = new Titanic("tcp://localhost:5555", verbose);
		
		// Create MDP client session with short timeout
		var client = new MDCliAPI("tcp://localhost:5555", verbose);
		client.timeout = 1000;		// 1 sec
		client.retries = 1;			// Only 1 retry
		
		var requestPipe = ZThread.attach(ctx, titanic.titanicRequest,"tcp://localhost:5555");
		ZThread.detach(titanic.titanicReply, "tcp://localhost:5555");
		ZThread.detach(titanic.titanicClose, "tcp://localhost:5555");
		
		var poller = new ZMQPoller();
		poller.registerSocket(requestPipe, ZMQ.ZMQ_POLLIN());
		
		// Main dispatcher loop
		while (true) {
			// We'll dispatch once per second, if there's no activity
			try {
				var res = poller.poll(1000 * 1000);		// 1 sec
			} catch (e:ZMQException) {
				if (!ZMQ.isInterrupted()) { 
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
				} else 
					log("W: interrupt received, sinking the titanic...");
				ctx.destroy();
				client.destroy();
				return;
			}
			if (poller.pollin(1)) {
				// Ensure message directory exists
				if (!FileSystem.exists(TITANIC_DIR))
					FileSystem.createDirectory(TITANIC_DIR);
				// Append UUID to queue, prefixed with "-" for pending
				var msg = ZMsg.recvMsg(requestPipe);
				if (msg == null)
					break;		// Interrupted
				var file = File.append(TITANIC_DIR + "/queue", false);
				var uuid = msg.pop().toString();
				file.writeString("-" + uuid);
				file.flush();
				file.close();
			}
			// Brute-force dispatcher
			if (FileSystem.exists(TITANIC_DIR + "/queue")) {
				try {
					var filec = File.getContent(TITANIC_DIR + "/queue");
					FileSystem.deleteFile(TITANIC_DIR + "/queue");
					var fileh = File.write(TITANIC_DIR + "/queue", false);
					var index = 0;
					while (index+33 <= filec.length) {
						var str = filec.substr(index, 33);
						var prefix = "-";
						// UUID is prefixed with '-' if still waiting
						if (str.charAt(0) == "-") {
							if (verbose)
								log("I: processing request " + str.substr(1));
							if (titanic.serviceSuccess(client, str.substr(1))) {
								// Mark queue entry as processed
								prefix = "+";
							}
						}
						fileh.writeString(prefix + str.substr(1));
						index += 33;
					}
					fileh.flush();
					fileh.close();	
				} catch (e:Dynamic) {
					log("E: error reading queue file " +e);
				}
			}
		}
		client.destroy();
		ctx.destroy();
	}
	
	/**
	 * Constructor
	 * @param	broker
	 * @param	?verbose
	 * @param	?logger
	 */
	public function new(broker:String, ?verbose:Bool, ?logger:Dynamic->Void) {
		this.broker = broker;
		this.verbose = verbose;
		if (logger != null) 
			log = logger;
		else
			log = neko.Lib.println;
	}
	

	/**
	 * Returns a new UUID as a printable String
	 * @param	?size
	 * @return
	 */
	private function generateUUID(?size:Int):String {
		if (size == null) size = 32;
		var nchars = UID.length;
		var uid = new StringBuf();
		for (i in 0 ... size) {
			uid.add(UID.charAt(ZHelpers.randof(nchars-1)));
		}
		return uid.toString();
	}
	
	/**
	 * Returns request filename for given UUID
	 * @param	uuid
	 * @return
	 */
	private function requestFilename(uuid:String):String {
		return TITANIC_DIR + "/" + uuid + ".req";
	}

	/**
	 * Returns reply filename for given UUID
	 * @param	uuid
	 * @return
	 */
	private function replyFilename(uuid:String):String {
		return TITANIC_DIR + "/" + uuid + ".rep";
	}
	
	/**
	 * Implements Titanic request service "titanic.request"
	 * @param	ctx
	 * @param	pipe
	 */
	public function titanicRequest(ctx:ZContext, pipe:ZMQSocket, broker:String) {
		var worker = new MDWrkAPI(broker, "titanic.request", verbose);
		var reply:ZMsg = null;
		
		while (true) {
			if (reply != null) trace("reply object:" + reply.toString());
			// Send reply if it's not null
			// and then get next request from broker
			var request = worker.recv(reply);
			if (request == null)
				break;		// Interrupted, exit
			
			// Ensure message directory exists
			if (!FileSystem.exists(TITANIC_DIR))
				FileSystem.createDirectory(TITANIC_DIR);
			
			// Generate UUID and save message to disk
			var uuid = generateUUID();
			var filename = requestFilename(uuid);
			var file = File.write(filename, false);
			ZMsg.save(request, file);
			file.close();
			request.destroy();
			// Send UUID through to message queue
			reply = new ZMsg();
			reply.addString(uuid);
			reply.send(pipe);
			
			// Now send UUID back to client
			// Done by the worker.recv() call at the top of the loop
			reply = new ZMsg();
			reply.addString("200");
			reply.addString(uuid);
		}
		worker.destroy();
	}
	
	/**
	 * Implements titanic reply service "titanic.reply"
	 */
	public function titanicReply(broker:String) {
		var worker = new MDWrkAPI(broker, "titanic.reply", verbose);
		var reply:ZMsg = null;
		
		while (true) {
			// Send reply if it's not null
			// and then get next request from broker
			var request = worker.recv(reply);
			if (request == null)
				break;		// Interrupted, exit
			
			// Ensure message directory exists
			if (!FileSystem.exists(TITANIC_DIR))
				FileSystem.createDirectory(TITANIC_DIR);
			
			// Generate UUID and save message to disk
			var uuid = request.popString();
			var reqfilename = requestFilename(uuid);
			var repfilename = replyFilename(uuid);
			if (FileSystem.exists(repfilename)) {
				var file = File.read(repfilename, false);
				reply = ZMsg.load(file);
				reply.pushString("200");
				file.close();
			} else {
				reply = new ZMsg();
				if (FileSystem.exists(reqfilename)) 
					reply.pushString("300");	// Pending
				else
					reply.pushString("400");
				request.destroy();	
			}
		}
		worker.destroy();
	}
	
	/**
	 * Implements titanic close service "titanic.close"
	 * @param	broker
	 */
	public function titanicClose(broker:String) {
		var worker = new MDWrkAPI(broker, "titanic.close", verbose);
		var reply:ZMsg = null;
		
		while (true) {
			// Send reply if it's not null
			// and then get next request from broker
			var request = worker.recv(reply);
			if (request == null)
				break;		// Interrupted, exit
			
			// Ensure message directory exists
			if (!FileSystem.exists(TITANIC_DIR))
				FileSystem.createDirectory(TITANIC_DIR);
			
			// Generate UUID and save message to disk
			var uuid = request.popString();
			var reqfilename = requestFilename(uuid);
			var repfilename = replyFilename(uuid);
			FileSystem.deleteFile(reqfilename);
			FileSystem.deleteFile(repfilename);
			
			request.destroy();
			reply = new ZMsg();
			reply.addString("200");
		}
		worker.destroy();
	}
	
	/**
	 * Attempt to process a single service request message, return true if successful
	 * @param	client
	 * @param	uuid
	 * @return
	 */
	public function serviceSuccess(client:MDCliAPI, uuid:String):Bool {
		// Load request message, service will be first frame
		var filename = requestFilename(uuid);
		var file = File.read(filename, false);
		var request:ZMsg = null;
		try {
			request = ZMsg.load(file);
			file.close();
		} catch (e:Dynamic) {
			log("E: Error loading file:" + filename + ", details:" + e);
			return false;
		}
		var service = request.pop();
		var serviceName = service.toString();
		
		// Use MMI protocol to check if service is available
		var mmiRequest = new ZMsg();
		mmiRequest.add(service);
		var mmiReply = client.send("mmi.service", mmiRequest);
		var serviceOK = (mmiReply != null && mmiReply.first().streq("200"));
		
		if (serviceOK) {
			// Now call requested service and store reply from service
			var reply = client.send(serviceName, request);
			if (reply != null) {
				filename = replyFilename(uuid);
				try {
					var file = File.write(filename, false);
					ZMsg.save(reply, file);
					file.close();
					return true;
				} catch (e:Dynamic) {
					log("E: Error writing file:" + filename + ", details:" + e);
					return false;
				}
			}
			reply.destroy();
		} else
			request.destroy();
		return false;
	}
	
}