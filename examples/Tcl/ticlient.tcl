#
#  Titanic client example
#  Implements client side of http://rfc.zeromq.org/spec:9

lappend auto_path .
package require MDClient 1.0

set verbose 0
foreach {k v} $argv {
    if {$k eq "-v"} { set verbose 1 }
}

#  Calls a TSP service
#  Returns reponse if successful (status code 200 OK), else NULL
#
proc s_service_call {session service request} {
    set reply [$session send $service $request]
    if {[llength $reply]} {
	set status [zmsg pop reply]
	switch -exact -- $status {
	    200 {
		return $reply
	    }
	    400 {
		puts "E: client fatal error, aborting"
		exit 1
	    }
	    500 {
		puts "E: server fatal error, aborting"
		exit 1
	    }
	}
    } else {
	puts "I: Interrupted or failed"
	exit 0 ;# Interrupted or failed
    }

    return {} ;# Didn't succeed, don't care why not
}

set session [MDClient new "tcp://localhost:5555" $verbose]

#  1. Send 'echo' request to Titanic
set request [list "echo" "Hello world"]
set reply [s_service_call $session "titanic.request" $request]

set uuid ""
if {[llength $reply]} {
    set uuid [zmsg pop reply]
    puts "I: request UUID [zmsg dump [list $uuid]]"
}

#  2. Wait until we get a reply
while {1} {
    after 100
    set request [list $uuid]
    set reply [s_service_call $session "titanic.reply" $request]

    if {[llength $reply]} {
	set reply_string [lindex $reply end]
	puts "Reply: $reply_string"

	#  3. Close request
	s_service_call $session "titanic.close" $request
	break
    } else {
	puts "I: no reply yet, trying again..."
	after 5000
    }
}

$session destroy
