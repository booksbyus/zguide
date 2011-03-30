--
--  mdp
--  Majordomo Protocol definitions
--
--  Author: Robert G. Jakabosky <bobby@sharedrealm.com>
--

module(...)

--  This is the version of MDP/Client we implement
MDPC_CLIENT          = "MDPC01"

--  This is the version of MDP/Worker we implement
MDPW_WORKER          = "MDPW01"

--  MDP/Server commands, as strings
MDPW_READY           = "\001"
MDPW_REQUEST         = "\002"
MDPW_REPLY           = "\003"
MDPW_HEARTBEAT       = "\004"
MDPW_DISCONNECT      = "\005"

mdps_commands = {
    "READY", "REQUEST", "REPLY", "HEARTBEAT", "DISCONNECT",
    ["\001"] = "READY",
    ["\002"] = "REQUEST",
    ["\003"] = "REPLY",
    ["\004"] = "HEARTBEAT",
    ["\005"] = "DISCONNECT",
}

