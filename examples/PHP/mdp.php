<?php

//  This is the version of MDP/Client we implement
define("MDPC_CLIENT", "MDPC01");

//  This is the version of MDP/Worker we implement
define("MDPW_WORKER", "MDPW01");

//  MDP/Server commands, as strings
define("MDPW_READY", "\001");
define("MDPW_REQUEST", "\002");
define("MDPW_REPLY", "\003");
define("MDPW_HEARTBEAT", "\004");
define("MDPW_DISCONNECT", "\005");