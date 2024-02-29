//
//  mdp.h
//  Majordomo Protocol definitions
//
#ifndef __MDP_H_INCLUDED__
#define __MDP_H_INCLUDED__
#include <string_view>
#include <string>

//  This is the version of MDP/Client we implement
static constexpr std::string_view k_mdp_client= "MDPC01";

//  This is the version of MDP/Worker we implement
static constexpr std::string_view k_mdpw_worker= "MDPW01";
//  MDP/Server commands, as strings
static constexpr std::string_view k_mdpw_ready = "\001";
static constexpr std::string_view k_mdpw_request ="\002";
static constexpr std::string_view k_mdpw_reply ="\003";
static constexpr std::string_view k_mdpw_heartbeat="\004";
static constexpr std::string_view k_mdpw_disconnect="\005";

static constexpr std::string_view mdps_commands [] = {
    "", "READY", "REQUEST", "REPLY", "HEARTBEAT", "DISCONNECT"
};


using ustring = std::basic_string<unsigned char>;
#endif

