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
import haxe.io.Bytes;
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

/**
 * Majordomo Protocol broker
 * A minimal implementation of http://rfc.zeromq.org/spec:7 and spec:8
 */
class MDBroker 
{

	public static function main() {
		var argArr = Sys.args();
		var verbose = (argArr.length > 1 && argArr[argArr.length - 1] == "-v");
		
		var log = Lib.println;
		
		var broker = new Broker(verbose, log);
		broker.bind("tcp://*:5555");
		
		var poller = new ZMQPoller();
		poller.registerSocket(broker.socket, ZMQ.ZMQ_POLLIN());
		// Get and process messages forever or until interrupted
		while (true) {
			try {
				var res = poller.poll(Constants.HEARTBEAT_INTERVAL);
			} catch (e:ZMQException) {
				if (!ZMQ.isInterrupted()) {
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
				} else {
					Lib.println("W: interrupt received, killing broker...");	
				}
				broker.destroy();
				return;
			}
			
			// Process next input message, if any
			if (poller.pollin(1)) {
				var msg = ZMsg.recvMsg(broker.socket);
				if (msg == null)
					break;		// Interrupted
				if (verbose)
					log("I: received message:" + msg.toString());
				var sender = msg.pop();
				var empty = msg.pop();
				var header = msg.pop();
				
				if (header.streq(MDP.MDPC_CLIENT))
					broker.processClient(sender, msg);
				else if (header.streq(MDP.MDPW_WORKER))
					broker.processWorker(sender, msg);
				else {
					log("E: invalid message:" + msg.toString());
					msg.destroy();
				}
				sender.destroy();
				empty.destroy();
				header.destroy();
			}
			// Disconnect and delete any expired workers
			// Send heartbeats to idle workers if needed
			if (Date.now().getTime() > broker.heartbeatAt) {
				broker.purgeWorkers();
				for (w in broker.waiting) {
					broker.sendWorker(w, MDP.MDPW_HEARTBEAT, null, null);
				}
				broker.heartbeatAt = Date.now().getTime() + Constants.HEARTBEAT_INTERVAL;
			}
		}
		if (ZMQ.isInterrupted())
			log("W: interrupt received, shutting down...");
		broker.destroy();	
	}
}

private class Constants {
	// We'd normally pull these from config data
	public static inline var HEARTBEAT_LIVENESS = 3;		// 3-5 is reasonable
	public static inline var HEARTBEAT_INTERVAL = 2500;	// msecs
	public static inline var HEARTBEAT_EXPIRY = HEARTBEAT_INTERVAL * HEARTBEAT_LIVENESS;	
}

/**
 * Internal class, managing a single service
 */
private class Service {
	
	/** Service name */
	public var name(default, null):String;
	
	/** List of client requests */
	public var requests:List<ZMsg>;
	
	/** List of waiting service workers */
	public var waiting:List<Worker>;
	
	/** How many workers the service has */
	public var numWorkers:Int;
	
	/**
	 * Constructor
	 * @param	name
	 */
	public function new(name:String) {
		this.name = name;
		this.requests = new List<ZMsg>();
		this.waiting = new List<Worker>();
		this.numWorkers = 0;
	}
	
	/**
	 * Destructor
	 */
	public function destroy() {
		for (r in requests) {
			r.destroy();
		}
		requests = null;
		waiting = null;
	}

	public function removeWorker(worker:Worker) {
		if (worker == null)
			return;
		waiting.remove(worker);
		numWorkers--;
	}
}

/**
 * Internal class, managing a single worker
 */
private class Worker {
	
	/** Identity of worker */
	public var identity(default, null):String;
	
	/** Address frame to route to */
	public var address(default, null):ZFrame;
	
	/** Owning service, if known */
	public var service:Service;
	
	/** Expires at unless heartbeat */
	public var expiry:Float;
	
	public function new(address:ZFrame, identity:String, ?service:Service, ?expiry:Float = 0.0) {
		this.address = address;
		this.identity = identity;
		this.service = service;
		this.expiry = expiry;
	}
	
	/**
	 * Deconstructor
	 */
	public function destroy() {
		address.destroy();
	}
	
	/**
	 * Return true if worker has expired and must be deleted
	 * @return
	 */
	public function expired():Bool {
		return expiry < Date.now().getTime();
	}
}

/**
 * Main class definining state and behaviour of a MDBroker
 */
private class Broker {
	
	/** Socket for clients and workers */
	public var socket(default, null):ZMQSocket;
	
	/** When to send heartbeat */
	public var heartbeatAt:Float;
	
	/** Hash of waiting workers */
	public var waiting(default, null):List<Worker>;
	
	// Private fields
	
	/** Our context */
	private var ctx:ZContext;
	
	/** Broker binds to this endpoint */
	private var endpoint:String;
	
	/** Hash of known services */
	private var services:Hash<Service>;
	
	/** Hash of known workers */
	private var workers:Hash<Worker>;
	
	/** Print activity to stdout */
	private var verbose:Bool;
	
	/** Logger function used in verbose mode */
	private var log:Dynamic->Void;
	
	/**
	 * Construct new broker
	 */
	public function new(?verbose:Bool = false, ?logger:Dynamic->Void)  
	{
		ctx = new ZContext();
		socket = ctx.createSocket(ZMQ_ROUTER);
		services = new Hash<Service>();
		workers = new Hash<Worker>();
		waiting = new List<Worker>();
		heartbeatAt = Date.now().getTime() + Constants.HEARTBEAT_INTERVAL;
		
		this.verbose = verbose;
		if (logger != null) 
			log = logger;
		else
			log = Lib.println;
		
	}
	
	/**
	 * Destructor
	 */
	public function destroy() {
		ctx.destroy();
	}
	
	/**
	 * Bind broker to endpoint, can call this multiple times
	 * We use a single socket for both clients and workers.
	 * @param	endpoint
	 */
	public function bind(endpoint:String) {
		socket.bind(endpoint);
		log("I: MDP broker/0.1.1 is active at " + endpoint);
	}
	
	/**
	 * Delete any idle workers that haven't pinged us in a while
	 */
	public function purgeWorkers() {
		for (w in waiting) {
			if (Date.now().getTime() < w.expiry)
				continue;		// Worker is alive, we're done here
			if (verbose)
				log("I: deleting expired worker: " + w.identity);
			deleteWorker(w, false);
		}
	}
	
	/**
	 * Locate or create a new service entry
	 * @param	name
	 * @return
	 */
	private function requireService(name:String):Service {
		if (name == null)
			return null;
		if (services.exists(name))
			return services.get(name);
		else {
			var srv = new Service(name);
			services.set(name, srv);
			if (verbose)
				log("I: Added service :" + name);
			return srv;	
		}
		return null;
	}
	
	/**
	 * Dispatch as many requests to waiting workers as possible
	 * @param	service
	 * @param	msg
	 */
	private function dispatchService(service:Service, ?msg:ZMsg) {
		// Tidy up workers first
		purgeWorkers();
		
		if (msg != null) 
			service.requests.add(msg);
		
		while (!service.waiting.isEmpty() && !service.requests.isEmpty()) {
			var worker = service.waiting.pop();
			waiting.remove(worker);
			var _msg = service.requests.pop();
			sendWorker(worker, MDP.MDPW_REQUEST, null, _msg);
		}
	}
	
	/**
	 * Handle internal service according to 8/MMI specification
	 * @param	serviceFrame
	 * @param	msg
	 * @return
	 */
	private function internalService(serviceFrame:ZFrame, msg:ZMsg) {
		var returnCode = "";
		if (serviceFrame == null || msg == null)
			return;
		if (serviceFrame.streq("mmi.service")) {
			var name = msg.last().toString();
			var service = services.get(name);
			returnCode = {
				if ((service != null) && (service.numWorkers > 0))
					"200";
				else
					"404";
			};
		} else
			returnCode = "501";
			
		msg.last().reset(Bytes.ofString(returnCode));
		
		// Remove & save client return envelope and insert the
		// protocol header and service name, then rewrap envelope.
		var client = msg.unwrap();
		msg.push(serviceFrame.duplicate());
		msg.pushString(MDP.MDPC_CLIENT);
		msg.wrap(client);
		msg.send(socket);
	}
	
	/**
	 * Creates worker if necessary
	 * @param	address
	 * @return
	 */
	private function requireWorker(address:ZFrame):Worker {
		if (address == null)
			return null;
		
		// workers Hash is keyed off worker identity
		var identity = address.strhex();
		var worker = workers.get(identity);
		
		if (worker == null) {
			worker = new Worker(address.duplicate(), identity);
			workers.set(identity, worker);
			if (verbose)
				log("I: registering new worker: " + identity);
		} 
		return worker;
	}
	
	/**
	 * Deletes worker from all data structures, and destroys worker object itself
	 * @param	worker
	 * @param	disconnect
	 */
	private function deleteWorker(worker:Worker, disconnect:Bool) {
		if (worker == null)
			return;
		if (disconnect)
			sendWorker(worker, MDP.MDPW_DISCONNECT, null, null);
		
		if (worker.service != null)
			worker.service.removeWorker(worker);
		waiting.remove(worker);
		workers.remove(worker.identity);
		worker.destroy();
	}
	
	/**
	 * Process message sent to us by a worker
	 * @param	sender
	 * @param	msg
	 */
	public function processWorker(sender:ZFrame, msg:ZMsg) {
		if (msg.size() < 1)
			return;	// At least, command
		var command = msg.pop();
		var identity = sender.strhex();
		var workerReady = workers.exists(identity);
		var worker = requireWorker(sender);
		
		if (command.streq(MDP.MDPW_READY)) {
			if (workerReady)		// Not first command in session
				deleteWorker(worker, true);		// Disconnect worker
			else if ( sender.size() >= 4 // Reserved for service name
				&& sender.toString().indexOf("mmi.") == 0)
				deleteWorker(worker, true);
			else {
				// Attach worker to service and mark as idle
				var serviceFrame = msg.pop();
				worker.service = requireService(serviceFrame.toString());
				worker.service.numWorkers++;
				waitingWorker(worker);
				serviceFrame.destroy();
			}
		} else if (command.streq(MDP.MDPW_REPLY)) {
			if (workerReady) {
				// Remove & save client return envelope and insert the
				// protocol header and service name, then rewrap envelope.
				var client = msg.unwrap();
				msg.pushString(worker.service.name);
				msg.pushString(MDP.MDPC_CLIENT);
				msg.wrap(client);
				msg.send(socket);
				waitingWorker(worker);
			} else 
				deleteWorker(worker, true);
		} else if (command.streq(MDP.MDPW_HEARTBEAT)) {
			if (workerReady) 
				worker.expiry = Date.now().getTime() + Constants.HEARTBEAT_EXPIRY;
			else
				deleteWorker(worker, true);
		} else if (command.streq(MDP.MDPW_DISCONNECT)) 
			deleteWorker(worker, false);
		else 
			log("E: invalid input message:" + msg.toString());
		
		if (msg != null)
			msg.destroy();	
	}
	
	/**
	 * Send message to worker
	 * If message is provided, sends that message. Does not
	 * destroy the message, this is the caller's job.
	 * @param	worker
	 * @param	command
	 * @param	option
	 * @param	msg
	 */
	public function sendWorker(worker:Worker, command:String, option:String, msg:ZMsg) {
		var _msg = { if (msg != null) msg.duplicate(); else new ZMsg(); };
		
		// Stack protocol envelope to start of message
		if (option != null)
			_msg.pushString(option);
		_msg.pushString(command);
		_msg.pushString(MDP.MDPW_WORKER);
		
		// Stack routing envelope to start of message
		_msg.wrap(worker.address.duplicate());
		
		if (verbose)
			log("I: sending " + MDP.MDPS_COMMANDS[command.charCodeAt(0)] + " to worker: " + _msg.toString());
		_msg.send(socket);	
	}
	
	/**
	 * This worker is now waiting for work
	 * @param	worker
	 */
	private function waitingWorker(worker:Worker) {
		waiting.add(worker);
		worker.service.waiting.add(worker);
		worker.expiry = Date.now().getTime() + Constants.HEARTBEAT_EXPIRY;
		dispatchService(worker.service);
	}
	
	public function processClient(sender:ZFrame, msg:ZMsg) {
		if (msg.size() < 2)		// Service name and body
			return;
		
		var serviceFrame = msg.pop();
		var service = requireService(serviceFrame.toString());
		
		// Set reply return address to client sender
		msg.wrap(sender.duplicate());
		if ( serviceFrame.size() >= 4 // Reserved for service name
				&& serviceFrame.toString().indexOf("mmi.") == 0)		
			internalService(serviceFrame, msg);
		else
			dispatchService(service, msg);
		serviceFrame.destroy();	
	}
}