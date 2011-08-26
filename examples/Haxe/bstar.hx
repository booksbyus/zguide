/**
 * (c) 2011 Richard J Smith
 *
 * Based on implementation of bstar in the ZeroMQ ZGuide:
 * http://github.com/imatix/zguide/blob/master/examples/C/bstar.c
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
import neko.Sys;
import neko.Lib;
import org.zeromq.ZContext;
import org.zeromq.ZLoop;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQException;
import org.zeromq.ZMQSocket;
import org.zeromq.ZMsg;
import org.zeromq.ZSocket;

// States we can be in at any time
private enum StateT {
	STATE_PRIMARY;		// Primary, waiting for peer to connect
	STATE_BACKUP;		// Backup, waiting for peer to connect
	STATE_ACTIVE;		// Active - accepting connections
	STATE_PASSIVE;		// Passive - not accepting connections
}

private enum EventT {
	PEER_PRIMARY;		// HA peer is pending primary
	PEER_BACKUP;		// HA peer is pending backup
	PEER_ACTIVE;		// HA peer is active
	PEER_PASSIVE;		// HA peer is passive
	CLIENT_REQUEST;		// Client makes request
}

/**
 * Shortcut typedef for method signature of BStar reactor handler functions
 */
typedef HandlerFunctionType = ZLoop->ZMQSocket->Dynamic->Int;

/**
 * Binary Star Reactor
 */
class BStar 
{

	/** We send state information every this often
	 * If peer doesn't respond in two heartbeats, it is 'dead'
	 */
	private static inline var BSTAR_HEARTBEAT = 100;

	/** Our context */
	private var ctx:ZContext;
	
	/** Reactor loop */
	public var loop(default, null):ZLoop;
	
	/** State publisher socket */
	private var statePub:ZMQSocket;
	
	/** State subscriber socket */
	private var stateSub:ZMQSocket;
	
	/** Current state */
	public var state(default, null):StateT;
	
	/** Current event */
	public var event(default, null):EventT;

	/** When peer is considered 'dead' */
	public var peerExpiry:Float;

	/** Voting socket handler */
	private var voterFn:HandlerFunctionType;
	
	/** Arguments for voting handler */
	private var voterArgs:Dynamic;
	
	/** Master socket handler, called when become Master */
	private var masterFn:HandlerFunctionType;
	
	/** Arguments for Master handler */
	private var masterArgs:Dynamic;
	
	/** Slave socket handler, called when become Slave */
	private var slaveFn:HandlerFunctionType;
	
	/** Arguments for slave handler */
	private var slaveArgs:Dynamic;
	
	/** Print activity to stdout */
	public var verbose:Bool;
	
	/** Logger function used in verbose mode */
	private var log:Dynamic->Void;
	
	/**
	 * BStar Constructor
	 * @param	isPrimary	True if this instance is the primary instance, else false if slave
	 * @param	local		Network address to bind the statePub socket of this instance to
	 * @param	remote		Network address to connect the stateSub socket of this instance to
	 * @param	?verbose	True to generate logging info
	 * @param	?logger		Logger function
	 */
	public function new(isPrimary:Bool, local:String, remote:String, ?verbose:Bool = false, ?logger:Dynamic->Void) 
	{
		// Initialise the binary star server
		ctx = new ZContext();
		loop = new ZLoop(logger);
		loop.verbose = verbose;
		state = { if (isPrimary) STATE_PRIMARY; else STATE_BACKUP; };
		
		// Create publisher for state going to peer
		statePub = ctx.createSocket(ZMQ_PUB);
		statePub.bind(local);
		
		// Create subscriber for state coming from peer
		stateSub = ctx.createSocket(ZMQ_SUB);
		stateSub.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
		stateSub.connect(remote);
		
		// Set up basic reactor events
		loop.registerTimer(BSTAR_HEARTBEAT, 0, sendState);
		var item = { socket: stateSub, event: ZMQ.ZMQ_POLLIN() };
		loop.registerPoller(item, recvState);
		
		this.verbose = verbose;
		if (logger != null) 
			log = logger;
		else
			log = Lib.println;
		
	}
	
	/**
	 * Destructor
	 * Cleans up internal ZLoop reactor object and ZContext objects.
	 */
	public function destroy() {
		if (loop != null)
			loop.destroy();
		if (ctx != null)
			ctx.destroy();	
	}

	/**
	 * Create socket, bind to local endpoint, and register as reader for
	 * voting. The socket will only be available if the Binary Star state
	 * machine allows it. Input on the socket will act as a "vote" in the
	 * Binary Star scheme.  We require exactly one voter per bstar instance.
	 * 
	 * @param	endpoint	Endpoint address
	 * @param	type		Socket Type to bind to endpoint
	 * @param	handler		Voter Handler method
	 * @param	args		Optional args to pass to Voter Handfler method when called.
	 * @return
	 */
	public function setVoter(endpoint:String, type:SocketType, handler:HandlerFunctionType, ?args:Dynamic):Bool {
		// Hold actual handler + arg so we can call this later
		var socket = ctx.createSocket(type);
		socket.bind(endpoint);
		voterFn = handler;
		voterArgs = args;
		return loop.registerPoller( { socket:socket, event:ZMQ.ZMQ_POLLIN() }, voterReady);
	}
	
	/**
	 * Sets handler method called when instance becomes Master
	 * @param	handler
	 * @param	?args
	 */
	public function setMaster(handler:HandlerFunctionType, ?args:Dynamic) {
		if (masterFn == null) {
			masterFn = handler;
			masterArgs = args;
		}
	}
	
	/**
	 * Sets handler method called when instance becomes Slave
	 * @param	handler
	 * @param	?args
	 */
	public function setSlave(handler:HandlerFunctionType, ?args:Dynamic) {
		if (slaveFn == null) {
			slaveFn = handler;
			slaveArgs = args;
		}
	}
	
	
	/**
	 * Executes finite state machine (apply event to this state)
	 * Returns true if there was an exception
	 * @return
	 */
	public function stateMachine():Bool
	{
		var exception = false;
		switch (state) {
			case STATE_PRIMARY:
				// Primary server is waiting for peer to connect
				// Accepts CLIENT_REQUEST events in this state
				switch (event) {
					case PEER_BACKUP:
						if (verbose)
							log("I: connected to backup (slave), ready as master");
						state = STATE_ACTIVE;
						if (masterFn != null) 
							masterFn(loop, null, masterArgs);
					case PEER_ACTIVE:
						if (verbose)
							log("I: connected to backup (master), ready as slave");
						state = STATE_PASSIVE;
						if (slaveFn != null) 
							slaveFn(loop, null, slaveArgs);
					case CLIENT_REQUEST:
						if (verbose)
							log("I: request from client, ready as master");
						state = STATE_ACTIVE;
						if (masterFn != null) 
							masterFn(loop, null, masterArgs);
					default:
				}
			case STATE_BACKUP:
				// Backup server is waiting for peer to connect
				// Rejects CLIENT_REQUEST events in this state
				switch (event) {
					case PEER_ACTIVE:
						if (verbose)
							log("I: connected to primary (master), ready as slave");
						state = STATE_PASSIVE;
						if (slaveFn != null) 
							slaveFn(loop, null, slaveArgs);
					case CLIENT_REQUEST:
						exception = true;
					default:	
				}
			case STATE_ACTIVE:
				// Server is active
				// Accepts CLIENT_REQUEST events in this state
				switch (event) {
					case PEER_ACTIVE:
						// Two masters would mean split-brain
						log("E: fatal error - dual masters, aborting");
						exception = true;
					default:
				}
			case STATE_PASSIVE:
				// Server is passive
				// CLIENT_REQUEST events can trigger failover if peer looks dead
				switch (event) {
					case PEER_PRIMARY:
						// Peer is restarting - I become active, peer will go passive
						if (verbose)
							log("I: primary (slave) is restarting, ready as master");
						state = STATE_ACTIVE;
					case PEER_BACKUP:
						// Peer is restarting - become active, peer will go passive
						if (verbose)
							log("I: backup (slave) is restarting, ready as master");
						state = STATE_ACTIVE;
					case PEER_PASSIVE:
						// Two passives would mean cluster would be non-responsive
						log("E: fatal error - dual slaves, aborting");
						exception = true;
					case CLIENT_REQUEST:
						// Peer becomes master if timeout as passed
						// It's the client request that triggers the failover
						if (Date.now().getTime() >= peerExpiry) {
							// If peer is dead, switch to the active state
							if (verbose)
								log("I: failover successful, ready as master");
							state = STATE_ACTIVE;
						} else {
							if (verbose)
								log("I: peer is active, so ignore connection");
							exception = true;
						}
					default:
				}
		}
		return exception;
	}
	
	/**
	 * Reactor event handler
	 * Publish our state to peer
	 * @param	loop
	 * @param	socket
	 * @param	arg
	 * @return
	 */
	public function sendState(loop:ZLoop, socket:ZMQSocket):Int {
		statePub.sendMsg(Bytes.ofString(Std.string(Type.enumIndex(state))));
		return 0;
	}
	
	/**
	 * Reactor event handler
	 * Receive state from peer, execute finite state machine.
	 * @param	loop
	 * @param	socket
	 * @return
	 */
	public function recvState(loop:ZLoop, socket:ZMQSocket):Int {
		var message = stateSub.recvMsg().toString();
		event = Type.createEnumIndex(EventT, Std.parseInt(message));
		peerExpiry = Date.now().getTime() + (2 * BSTAR_HEARTBEAT);		
		return {
			if (stateMachine())
				-1;		// Error, so exit
			else
				0;
		};
	}
	
	/**
	 * Application wants to speak to us, see if it's possible
	 * @param	loop
	 * @param	socket
	 * @return
	 */
	public function voterReady(loop:ZLoop, socket:ZMQSocket):Int {
		// If server can accept input now, call application handler
		event = CLIENT_REQUEST;
		if (stateMachine()) {
			// Destroy waiting message, no-one to read it
			var msg = socket.recvMsg();
		} else {
			if (verbose)
				log("I: CLIENT REQUEST");
			voterFn(loop, socket, voterArgs);
		}
		return 0;
	}
	
	/**
	 * Start the reactor, ends if a callback function returns -1, or the
	 * process receives SIGINT or SIGTERM
	 * @return 0 if interrupted or invalid, -1 if cancelled by handler
	 */
	public function start():Int {
		if (voterFn != null && loop != null)
			return loop.start();
		else
			return 0;
	}
	
}