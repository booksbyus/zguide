/**
 * (c) 2011 Richard J Smith
 *
 * This file is part of ZGuide
 *
 * ZGuide is free software; you can redistribute it and/or modify it under
 * the terms of the Lesser GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * ZGuide is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * Lesser GNU General Public License for more details.
 *
 * You should have received a copy of the Lesser GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package ;
import haxe.Stack;
import neko.Lib;
import org.zeromq.ZContext;
import org.zeromq.ZFrame;
import org.zeromq.ZMQ;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQException;

import MDP;

/**
 * Majordomo Protocol Client API
 * Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7
 */
class MDCliAPI 
{

	/** Request timeout (in msec) */
	public var timeout:Int;
	
	/** Request #retries */
	public var retries:Int;
	
	// Private instance fields 
	
	/** Our context */
	private var ctx:ZContext;
	
	/** Connection string to reach broker */
	private var broker:String;
	
	/** Socket to broker */
	private var client:ZMQSocket;
	
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
		ctx = new ZContext();
		this.broker = broker;
		this.verbose = verbose;
		this.timeout = 2500;		// msecs
		this.retries = 3;			// before we abandon
		if (logger != null) 
			log = logger;
		else
			log = Lib.println;
			
		connectToBroker();
	}
	
	/**
	 * Connect or reconnect to broker
	 */
	public function connectToBroker() {
		if (client != null) 
			client.close();
		client = ctx.createSocket(ZMQ_REQ);
		client.setsockopt(ZMQ_LINGER, 0);
		client.connect(broker);
		if (verbose)
			log("I: client connecting to broker at " + broker + "...");
	}
	
	/**
	 * Destructor
	 */
	public function destroy() {
		ctx.destroy();
	}
	
	/**
	 * Send request to broker and get reply by hook or crook.
	 * Takes ownership of request message and destroys it when sent.
	 * Returns the reply message or NULL if there was no reply after #retries
	 * @param	service
	 * @param	request
	 * @return
	 */
	public function send(service:String, request:ZMsg):ZMsg {
		// Prefix request with MDP protocol frames
		// Frame 1: "MDPCxy" (six bytes, MDP/Client)
		// Frame 2: Service name (printable string)
		request.push(ZFrame.newStringFrame(service));
		request.push(ZFrame.newStringFrame(MDP.MDPC_CLIENT));
		if (verbose) {
			log("I: send request to '" + service + "' service:");
			log(request.toString());
		}
		
		var retries_left = retries;
				
		var poller = new ZMQPoller();

		while (retries_left > 0 && !ZMQ.isInterrupted()) {
			// We send a request, then we work to get a reply
			var msg = request.duplicate();
			msg.send(client);
			
			var expect_reply = true;
			while (expect_reply) {
				poller.registerSocket(client, ZMQ.ZMQ_POLLIN());
				// Poll socket for a reply, with timeout
				try {
					var res = poller.poll(timeout * 1000);
				} catch (e:ZMQException) {
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
					ctx.destroy();
					return null;
				}
				// If we got a reply, process it
				if (poller.pollin(1)) {
					// We got a reply from the server, must match sequence
					var msg = ZMsg.recvMsg(client);
					if (msg == null)
						break;		// Interrupted
					if (verbose)
						log("I: received reply:" + msg.toString());
					if (msg.size() < 3)
						break;		// Don't try to handle errors
					var header = msg.pop();
					if (!header.streq(MDP.MDPC_CLIENT))
						break;		// Assert
					header.destroy();
					
					var reply_service = msg.pop();
					if (!reply_service.streq(service))
						break;		// Assert
					reply_service.destroy();
					request.destroy();
					return msg;		// Success
				} else if (--retries_left > 0) {
					if (verbose)
						log("W: no reply, reconnecting...");
					// Reconnect , and resend message
					connectToBroker();
					msg = request.duplicate();
					msg.send(client);
				} else {
					if (verbose)
						log("E: permanent error, abandoning");
					break;
				}
				poller.unregisterAllSockets();
			}
		}
		return null;
		
	}
	
}