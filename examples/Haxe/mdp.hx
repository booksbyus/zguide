package ;

/**
 * Majordomo Protocol definitions
 * 
 * Based on http://github.com/imatix/zguide/blob/master/examples/C/mdp.h
 */
class MDP 
{

	/** This is the version of MDP/Client we implement */
	public static inline var MDPC_CLIENT = "MDPC01";
	
	/** This is the version of MDP/Worker we implement */
	public static inline var MDPW_WORKER = "MDPW01";
	
	// MDP/Server commands, as strings
	public static inline var MDPW_READY = String.fromCharCode(1);
	public static inline var MDPW_REQUEST = String.fromCharCode(2);
	public static inline var MDPW_REPLY = String.fromCharCode(3);
	public static inline var MDPW_HEARTBEAT = String.fromCharCode(4);
	public static inline var MDPW_DISCONNECT = String.fromCharCode(5);
	
	public static inline var MDPS_COMMANDS = [null, "READY", "REQUEST", "REPLY", "HEARTBEAT", "DISCONNECT"];
}
