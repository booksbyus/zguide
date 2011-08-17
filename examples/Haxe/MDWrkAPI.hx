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

import MDP;
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

/**
 * Majordomo Protocol Worker API
 * Implements the MDP/Worker spec at http://rfc.zeromq.org/spec:7
 */
class MDWrkAPI 
{
	/** When to send HEARTBEAT */
	public var heartbeatAt:Float;
	
	/** Reconnect delay, in msecs */
	public var reconnect:Int;
	
		
	// Private instance fields 
	
	/** Our context */
	private var ctx:ZContext;
	
	/** Connection string to broker */
	private var broker:String;
	
	/** Socket to broker */
	private var worker:ZMQSocket;
	
	/** Defines service string provided by this worker */
	private var service:String;
	
	/** Print activity to stdout */
	private var verbose:Bool;
	
	/** Logger function used in verbose mode */
	private var log:Dynamic->Void;
	
	/** How many attempts left */
	private var liveness:Int;
	
	/** Heartbeat delay, in msecs */
	private var heartbeat:Int;
		
	/** Internal state */
	private var expect_reply:Bool;
	
	/** Return address frame, if any */
	private var replyTo:ZFrame;
	
	/** Reliability parameters */
	private static inline var HEARTBEAT_LIVENESS = 3;
	
	/**
	 * Constructor
	 * @param	broker
	 * @param	service
	 * @param	?verbose
	 * @param	?logger
	 */
	public function new(broker:String, service:String, ?verbose:Bool = false, ?logger:Dynamic->Void) {
		ctx = new ZContext();
		this.broker = broker;
		this.service = service;
		this.verbose = verbose;
		this.heartbeat = 2500;		// msecs
		this.reconnect = 2500;		// msecs
		if (logger != null) 
			log = logger;
		else
			log = neko.Lib.println;
		expect_reply = false;
		connectToBroker();	
	}
	
	/**
	 * Connect or reconnect to broker
	 */
	public function connectToBroker() {
		if (worker != null) 
			worker.close();
		worker = ctx.createSocket(ZMQ_DEALER);
		worker.setsockopt(ZMQ_LINGER, 0);
		worker.connect(broker);
		if (verbose)
			log("I: worker connecting to broker at " + broker + "...");
			
		sendToBroker(MDP.MDPW_READY, service);
		
		// If liveness hits zero, queue is considered disconnected
		liveness = HEARTBEAT_LIVENESS;
		heartbeatAt = Date.now().getTime() + heartbeat;
	}
	
	/**
	 * Destructor
	 */
	public function destroy() {
		ctx.destroy();
	}
	
	/**
	 * Send message to broker
	 * If no msg is provided, creates one internally
	 * 
	 * @param	command
	 * @param	option
	 * @param	?msg
	 */
	public function sendToBroker(command:String, option:String, ?msg:ZMsg) {
		var _msg:ZMsg = { if (msg != null) msg.duplicate(); else new ZMsg(); }
		
		// Stack protocol envelope to start of message
		if (option != null) {
			_msg.pushString(option);
		}
		_msg.pushString(command);
		_msg.pushString(MDP.MDPW_WORKER);
		_msg.pushString("");
		
		if (verbose)
			log("I: sending " + MDP.MDPS_COMMANDS[command.charCodeAt(0)] + " to broker " + _msg.toString());
		
		_msg.send(worker);	
	}
	
	/**
	 * Send reply, if any, to broker and wait for next request
	 * 
	 * @param	reply	Message to send back. Destroyed if send if successful
	 * @return	ZMsg	Returns if there is a request to process
	 */
	public function recv(?reply:ZMsg):ZMsg {
		if (reply == null && expect_reply)
			return null;
		if (reply != null) {	
			if (replyTo == null)
				return null;	// Cannot send if we dont know which client to reply to
			reply.wrap(replyTo);	
			sendToBroker(MDP.MDPW_REPLY, null, reply);
			reply.destroy();
		}
		expect_reply = true;
		
		var poller = new ZMQPoller();
		poller.registerSocket(worker, ZMQ.ZMQ_POLLIN());
		while (true) {
			// Poll socket for a reply, with timeout
			try {
				var res = poller.poll(heartbeat * 1000);
			} catch (e:ZMQException) {
				if (!ZMQ.isInterrupted()) {
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
				} else {
					Lib.println("W: interrupt received, killing worker...");	
				}
				ctx.destroy();
				return null;
			}
			if (poller.pollin(1)) {
				var msg = ZMsg.recvMsg(worker);
				if (msg == null)
					break;		// Interrupted
				if (verbose)
					log("I: received message from broker:" + msg.toString());
				liveness = HEARTBEAT_LIVENESS;
				
				if (msg.size() < 2)
					return null;	// Don't handle errors, just quit quietly
				msg.pop();
				var header = msg.pop();
				if (!header.streq(MDP.MDPW_WORKER))
					return null;	// Don't handle errors, just quit quietly
				header.destroy();
				var command = msg.pop();
				if (command.streq(MDP.MDPW_REQUEST)) {
					// We should pop and save as many addresses as there are
					// up to a null part, but for now, just save one...
					replyTo = msg.unwrap();
					command.destroy();
					return msg;		// We have a request to process
				} else if (command.streq(MDP.MDPW_HEARTBEAT)) {
					// Do nothing for heartbeats
				} else if (command.streq(MDP.MDPW_DISCONNECT)) {
					connectToBroker();
					poller.unregisterAllSockets();
					poller.registerSocket(worker, ZMQ.ZMQ_POLLIN());
				} else {
					log("E: invalid input message:" + msg.toString());
				}
				command.destroy();
				msg.destroy();
			} else if (--liveness == 0) {
				if (verbose)
					log("W: disconnected from broker - retrying...");
				Sys.sleep(reconnect / 1000);
				connectToBroker();
				poller.unregisterAllSockets();
				poller.registerSocket(worker, ZMQ.ZMQ_POLLIN());
			}
			// Send HEARTBEAT if it's time
			if (Date.now().getTime() > heartbeatAt) {
				sendToBroker(MDP.MDPW_HEARTBEAT, null, null);
				heartbeatAt = Date.now().getTime() + heartbeat;
			}
		}
		return null;
	}	
}