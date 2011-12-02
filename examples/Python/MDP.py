"""Majordomo Protocol definitions"""
#  This is the version of MDP/Client we implement
C_CLIENT = "MDPC01"

#  This is the version of MDP/Worker we implement
W_WORKER = "MDPW01"

#  MDP/Server commands, as strings
W_READY         =   "\001"
W_REQUEST       =   "\002"
W_REPLY         =   "\003"
W_HEARTBEAT     =   "\004"
W_DISCONNECT    =   "\005"

commands = [None, "READY", "REQUEST", "REPLY", "HEARTBEAT", "DISCONNECT"]
