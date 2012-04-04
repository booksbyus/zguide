# Majordomo Protocol definitions

package provide mdp 1.0

namespace eval ::mdp {
    # This is the version of MDP/Client we implement
    variable MDPC_CLIENT "MDPC01"
    # This is the version of MDP/Worker we implement
    variable MDPW_WORKER "MDPW01"
    # MDP/Server commands, as strings
    variable MDPW_COMMAND
    set MDPW_COMMAND(READY)      "\001"
    set MDPW_COMMAND(REQUEST)    "\002"
    set MDPW_COMMAND(REPLY)      "\003"
    set MDPW_COMMAND(HEARTBEAT)  "\004"
    set MDPW_COMMAND(DISCONNECT) "\005"

    variable contextid 0
    variable socketid 0

    proc socketid {} {
	variable socketid
	return [incr socketid]
    }

    proc contextid {} {
	variable contextid
	return [incr contextid]
    }

    variable HEARTBEAT_LIVENESS  3       ;#  3-5 is reasonable
    variable HEARTBEAT_INTERVAL  2500    ;#  msecs
    variable HEARTBEAT_EXPIRY    [expr {$HEARTBEAT_INTERVAL * $HEARTBEAT_LIVENESS}]
}
