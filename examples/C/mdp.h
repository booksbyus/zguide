//
//  mdp.h
//  Majordomo Protocol definitions
//
#ifndef __MDP_H_INCLUDED__
#define __MDP_H_INCLUDED__

//  This is the version of MDP/Client we implement
#define MDPC_CLIENT         "MDPC01"

//  This is the version of MDP/Worker we implement
#define MDPS_WORKER         "MDPW01"

//  MDP/Server commands, as strings
#define MDPS_READY          "\001"
#define MDPS_REQUEST        "\002"
#define MDPS_REPLY          "\003"
#define MDPS_HEARTBEAT      "\004"
#define MDPS_DISCONNECT     "\005"

#endif

