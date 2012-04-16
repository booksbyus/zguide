# =====================================================================
# kvmsg - key-value message class for example applications

# ---------------------------------------------------------------------
# Copyright (c) 1991-2011 iMatix Corporation <www.imatix.com>
# Copyright other contributors as noted in the AUTHORS file.

# Tcl port by Jos Decoster <jos.decoster@gmail.com>

# This file is part of the ZeroMQ Guide: http://zguide.zeromq.org

# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.

# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public
# License along with this program. If not, see
# <http://www.gnu.org/licenses/>.
# =====================================================================

lappend auto_path .

package require TclOO
package require uuid
package require zmq

package provide KVMsg 1.0

#  Keys are short strings
set KVMSG_KEY_MAX   255

#  Message is formatted on wire as 4 frames:
#  frame 0: key (0MQ string)
#  frame 1: sequence (8 bytes, network order)
#  frame 2: uuid (blob, 16 bytes)
#  frame 3: properties (0MQ string)
#  frame 4: body (blob)
set FRAME_KEY       0
set FRAME_SEQ       1
set FRAME_UUID      2
set FRAME_PROPS     3
set FRAME_BODY      4
set KVMSG_FRAMES    5

oo::class create KVMsg {

    variable frame key props

    #  Constructor, sets sequence as provided
    constructor {{isequence 0}} {
	set frame [list]
	#props array
	my set_sequence $isequence
    }

    destructor {
    }

    method set_frame {iframe} {
	set frame $iframe
    }

    method set_props {ipropsnm} {
	upvar $ipropsnm iprops
	unset -nocomplain props
	foreay {k v} [array get iprops] {
	    set props($k) $v
	}
    }

    #  Serialize list of properties to a message frame
    method encode_props {} {
	while {[llength $frame] < $::FRAME_PROPS} {
	    lappend frame {}
	}
	if {[array size props]} {
	    set s ""
	    foreach k [lsort -dictionary [array names props]] {
		append s "$k=$props($k)\n"
	    }
	    lset frame $::FRAME_PROPS $s
	}
    }

    #  Rebuild properties list from message frame
    method decode_props {} {
	unset -nocomplain props
	foreach s [split [string trimright [lindex $frame $::FRAME_PROPS] \n] \n] {
	    lassign [split $s =] k v
	    set props($k) $v
	}
    }

    #  Create duplicate of kvmsg
    method dup {} {
	set kvmsg [KVMsg new 0]
	$kvmsg set_frame $frame
	$kvmsg set_props props
	return $kvmsg
    }

    #  Reads key-value message from socket
    method recv {socket} {
	set frame [list]
	#  Read all frames off the wire
	for {set frame_nbr 0} {$frame_nbr < $::KVMSG_FRAMES} {incr frame_nbr} {
	    lappend frame [$socket recv]
	    #  Verify multipart framing
	    if {![$socket getsockopt RCVMORE]} {
		break
	    }
	}
	my decode_props
    }

    #  Send key-value message to socket; any empty frames are sent as such.
    method send {socket} {
	my encode_props
	for {set frame_nbr 0} {$frame_nbr < $::KVMSG_FRAMES} {incr frame_nbr} {
	    if {$frame_nbr == ($::KVMSG_FRAMES - 1)} {
		$socket send [lindex $frame $frame_nbr]
	    } else {
		$socket sendmore [lindex $frame $frame_nbr]
	    }
	}
    }

    #  Return key from last read message, if any, else NULL
    method key {} {
	if {[llength $frame] > $::FRAME_KEY} {
	    if {![info exists key]} {
		set size [string length [lindex $frame $::FRAME_KEY]]
		if {$size > $::KVMSG_KEY_MAX} {
		    set size $::KVMSG_KEY_MAX
		}
		set key [string range [lindex $frame $::FRAME_KEY] 0 [expr {$size - 1}]]
	    }
	    return $key
	} else {
	    return {}
	}
    }

    #  Return sequence nbr from last read message, if any
    method sequence {} {
	if {[llength $frame] > $::FRAME_SEQ} {
	    set s [lindex $frame $::FRAME_SEQ]
	    if {[string length $s] != 8} {
		error "sequence frame must have length 8"
	    }
	    binary scan [lindex $frame $::FRAME_SEQ] W r
	    return $r
	} else {
	    return 0
	}
    }

    #  Return UUID from last read message, if any, else NULL
    method body {} {
	if {[llength $frame] > $::FRAME_UUID} {
	    return [lindex $frame $::FRAME_UUID]
	} else {
	    return {}
	}
    }

    #  Return body from last read message, if any, else NULL
    method body {} {
	if {[llength $frame] > $::FRAME_BODY} {
	    return [lindex $frame $::FRAME_BODY]
	} else {
	    return {}
	}
    }

    #  Return body size from last read message, if any, else zero
    method size {} {
	if {[llength $frame] > $::FRAME_BODY} {
	    return [string length [lindex $frame $::FRAME_BODY]]
	} else {
	    return {}
	}
    }

    #  Set message key as provided
    method set_key {ikey} {
	while {[llength $frame] <= $::FRAME_KEY} {
	    lappend frame {}
	}
	lset frame $::FRAME_KEY $ikey
    }

    #  Set message sequence number
    method set_sequence {isequence} {
	while {[llength $frame] <= $::FRAME_SEQ} {
	    lappend frame {}
	}
	set sequence [binary format W $isequence]
	lset frame $::FRAME_SEQ $sequence
    }

    #  Set message UUID to generated value
    method set_uuid {} {
	while {[llength $frame] <= $::FRAME_UUID} {
	    lappend frame {}
	}
	lset frame $::FRAME_UUID [uuid::uuid generate]
    }

    #  Set message body
    method set_body {ibody} {
	while {[llength $frame] <= $::FRAME_BODY} {
	    lappend frame {}
	}
	lset frame $::FRAME_BODY $ibody
    }

    #  Set message key using printf format
    method fmt_key {format args} {
	my set_key [format $format {*}$args]
    }

    #  Set message body using printf format
    method fmt_body {format args} {
	my set_body [format $format {*}$args]
    }

    #  Get message property, if set, else ""
    method get_prop {name} {
	if {[info exists props($name)]} {
	    return $props($name)
	}
	return ""
    }

    #  Set message property
    #  Names cannot contain '='.
    method set_prop {name value} {
	if {[string first "=" $name] >= 0} {
	    error "property name can not contain a '=' character"
	}
	set props($name) $value
    }

    #  Store entire kvmsg into hash map, if key/value are set.
    #  Nullifies kvmsg reference, and destroys automatically when no longer
    #  needed. If value is empty, deletes any previous value from store.
    method store {hashnm} {
	upvar $hashnm hash
	if {[my size]} {
	    if {[info exists hash([my key])]} {
		$hash([my key]) destroy
	    }
	    set hash([my key]) [self]
	} else {
	    if {[info exists hash([my key])]} {
		$hash([my key]) destroy
		unset -nocomplain hash([my key])
	    }
	}
    }

    #  Dump message to stderr, for debugging and tracing
    method dump {} {
	set rt ""
	append rt [format {[seq:%lld]} [my sequence]]
	append rt [format {[key:%s]} [my key]]
	append rt [format {[size:%d] } [my size]]
	if {[array size props]} {
	    append rt "\["
	}
	foreach k [lsort -dictionary [array names props]] {
	    append rt "$k=$props($k);"
	}
	if {[array size props]} {
	    append rt "\]"
	}
	set size [my size]
	set body [my body]
	for {set i 0} {$i < $size} {incr i} {
	    set c [lindex $body $i]
	    if {[string is ascii $c]} {
		append rt $c
	    } else {
		append rt [binary scan H2 $c]
	    }
	}
	return $rt
    }
}

namespace eval ::KVMsgTest {
    proc test {verbose} {

	puts -nonewline " * kvmsg: "

	#  Prepare our context and sockets
	zmq context context
	set os [zmq socket output context DEALER]
	output bind "ipc://kvmsg_selftest.ipc"
	set is [zmq socket input context DEALER]
	input connect "ipc://kvmsg_selftest.ipc"

	#  Test send and receive of simple message
	set kvmsg [KVMsg new 1]
	$kvmsg set_key "key"
	$kvmsg set_uuid
	$kvmsg set_body "body"
	if {$verbose} {
	    puts [$kvmsg dump]
	}

	$kvmsg send $os
	$kvmsg store kvmap

	$kvmsg recv $is
	if {$verbose} {
	    puts [$kvmsg dump]
	}
	if {[$kvmsg key] ne "key"} {
	    error "Unexpected key: [$kvmsg key]"
	}
	$kvmsg destroy

	#  Test send and receive of message with properties
	set kvmsg [KVMsg new 2]
	$kvmsg set_prop "prop1" "value1"
	$kvmsg set_prop "prop2" "value2"
	$kvmsg set_prop "prop3" "value3"
	$kvmsg set_key "key"
	$kvmsg set_uuid
	$kvmsg set_body "body"
	if {$verbose} {
	    puts [$kvmsg dump]
	}
	$kvmsg send $os

	$kvmsg recv $is
	if {$verbose} {
	    puts [$kvmsg dump]
	}
	if {[$kvmsg key] ne "key"} {
	    error "Unexpected key: [$kvmsg key]"
	}
	if {[$kvmsg get_prop "prop2"] ne "value2"} {
	    error "Unexpected property value: [$kvmsg get_prop "prop2"]"
	}
	$kvmsg destroy

	#  Shutdown and destroy all objects
	input close
	output close
	context term

	puts "OK"
    }
}

#::KVMsgTest::test 1
