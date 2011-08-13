/**
 * (c) 2011 Richard J Smith
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