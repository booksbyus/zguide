package ;
import haxe.io.Bytes;
import haxe.Stack;
import neko.Sys;
import neko.Lib;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQPoller;
import org.zeromq.ZMQException;
import org.zeromq.ZMsg;
import org.zeromq.ZSocket;


/**
 * Binary Star Server
 * @author Richard J Smith
 * 
 * @see http://zguide.zeromq.org/page:all#Binary-Star-Implementation
 */

class BStarSrv 
{

	private static inline var HEARTBEAT = 100;

	/** Current state */
	public var state:StateT;
	
	/** Current event */
	public var event:EventT;
	
	/** When peer is considered 'dead' */
	public var peerExpiry:Float;
	
	/**
	 * BStarSrv constructor
	 * @param state	Initial state
	 */
	public function new(state:StateT) {
		this.state = state;
	}
	
	/**
	 * Main binary star server loop
	 */
	public function run() {
		var ctx = new ZContext();
		var statePub = ctx.createSocket(ZMQ_PUB);
		var stateSub = ctx.createSocket(ZMQ_SUB);
		var frontend = ctx.createSocket(ZMQ_ROUTER);
		
		switch (state) {
			case STATE_PRIMARY:
				Lib.println("I: primary master, waiting for backup (slave)");
				frontend.bind("tcp://*:5001");
				statePub.bind("tcp://*:5003");
				stateSub.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
				stateSub.connect("tcp://localhost:5004");
			case STATE_BACKUP:
				Lib.println("I: backup slave, waiting for primary (master)");
				frontend.bind("tcp://*:5002");
				statePub.bind("tcp://*:5004");
				stateSub.setsockopt(ZMQ_SUBSCRIBE, Bytes.ofString(""));
				stateSub.connect("tcp://localhost:5003");
			default:	
				ctx.destroy();
				return;
		}
		// Set timer for next outgoing state message
		var sendStateAt = Date.now().getTime() + HEARTBEAT;
		
		var poller = new ZMQPoller();
		poller.registerSocket(frontend, ZMQ.ZMQ_POLLIN());
		poller.registerSocket(stateSub, ZMQ.ZMQ_POLLIN());
		
		while (!ZMQ.isInterrupted()) {
			var timeLeft = Std.int(sendStateAt - Date.now().getTime());
			if (timeLeft < 0)
				timeLeft = 0;
			try {
				var res = poller.poll(timeLeft * 1000);		// Convert timeout to microseconds
			} catch (e:ZMQException) {
				if (!ZMQ.isInterrupted()) {
					trace("ZMQException #:" + e.errNo + ", str:" + e.str());
					trace (Stack.toString(Stack.exceptionStack()));
				} else {
					Lib.println("W: interrupt received, killing server...");	
				}
				ctx.destroy();
				return;
			}
			if (poller.pollin(1)) {
				// Have a client request
				var msg = ZMsg.recvMsg(frontend);
				event = CLIENT_REQUEST;
				if (!stateMachine())
					// Answer client by echoing request back
					msg.send(frontend);		// Pretend do some work and then reply
				else
					msg.destroy();
			}
			if (poller.pollin(2)) {
				// Have state from our peer, execute as event
				var message = stateSub.recvMsg().toString();
				event = Type.createEnumIndex(EventT, Std.parseInt(message));
				if (stateMachine())
					break;		// Error, so exit
				peerExpiry = Date.now().getTime() + (2 * HEARTBEAT);	
			}
			// If we timed-out, send state to peer
			if (Date.now().getTime() >= sendStateAt) {
				statePub.sendMsg(Bytes.ofString(Std.string(Type.enumIndex(state))));
				sendStateAt = Date.now().getTime() + HEARTBEAT;
			}
		}
		ctx.destroy();
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
						Lib.println("I: connected to backup (slave), ready as master");
						state = STATE_ACTIVE;
					case PEER_ACTIVE:
						Lib.println("I: connected to backup (master), ready as slave");
						state = STATE_PASSIVE;
					default:
				}
			case STATE_BACKUP:
				// Backup server is waiting for peer to connect
				// Rejects CLIENT_REQUEST events in this state
				switch (event) {
					case PEER_ACTIVE:
						Lib.println("I: connected to primary (master), ready as slave");
						state = STATE_PASSIVE;
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
						Lib.println("E: fatal error - dual masters, aborting");
						exception = true;
					default:
				}
			case STATE_PASSIVE:
				// Server is passive
				// CLIENT_REQUEST events can trigger failover if peer looks dead
				switch (event) {
					case PEER_PRIMARY:
						// Peer is restarting - become active, peer will go passive
						Lib.println("I: primary (slave) is restarting, ready as master");
						state = STATE_ACTIVE;
					case PEER_BACKUP:
						// Peer is restarting - become active, peer will go passive
						Lib.println("I: backup (slave) is restarting, ready as master");
						state = STATE_ACTIVE;
					case PEER_PASSIVE:
						// Two passives would mean cluster would be non-responsive
						Lib.println("E: fatal error - dual slaves, aborting");
						exception = true;
					case CLIENT_REQUEST:
						// Peer becomes master if timeout as passed
						// It's the client request that triggers the failover
						if (Date.now().getTime() >= peerExpiry) {
							// If peer is dead, switch to the active state
							Lib.println("I: failover successful, ready as master");
							state = STATE_ACTIVE;
						} else {
							Lib.println("I: peer is active, so ignore connection");
							exception = true;
						}
					default:
				}
		}
		return exception;
	}
	
	public static function main() {
		
		Lib.println("** BStarSrv (see: http://zguide.zeromq.org/page:all#Binary-Star-Implementation)");
		
		var state:StateT = null;
		var argArr = Sys.args();
		if (argArr.length > 1 && argArr[argArr.length - 1] == "-p") {
			state = STATE_PRIMARY;
		} else if (argArr.length > 1 && argArr[argArr.length - 1] == "-b") {
			state = STATE_BACKUP;
		} else {
			Lib.println("Usage: bstartsrv { -p | -b }");
			return;
		}
		
		var bstarServer = new BStarSrv(state);
		bstarServer.run();
	}
	
}

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
