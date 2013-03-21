//  Majordomo Protocol definitions

//  Author: iano <scaly.iano@gmail.com>

package main

const (
	//  This is the version of MDP/Client we implement
	MDPC_CLIENT = "MDPC01"

	//  This is the version of MDP/Worker we implement
	MDPW_WORKER = "MDPW01"

	//  MDP/Server commands, as strings
	MDPW_READY      = "\001"
	MDPW_REQUEST    = "\002"
	MDPW_REPLY      = "\003"
	MDPW_HEARTBEAT  = "\004"
	MDPW_DISCONNECT = "\005"
)

var Commands = []string{"", "READY", "REQUEST", "REPLY", "HEARTBEAT", "DISCONNECT"}
