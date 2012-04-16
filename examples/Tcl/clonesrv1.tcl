#
#  Clone server Model One
#

lappend auto_path .
package require KVSimple

#  Prepare our context and publisher socket
zmq context context
set pub [zmq socket publisher context PUB]
$pub bind "tcp://*:5556"
after 200

set sequence 0
expr srand([pid])

while {1} {
    #  Distribute as key-value message
    set kvmsg [KVSimple new [incr sequence]]
    $kvmsg set_key [expr {int(rand()*10000)}]
    $kvmsg set_body [expr {int(rand()*1000000)}]
    $kvmsg send $pub
    $kvmsg store kvmap
    puts [$kvmsg dump]
    after 500
}

$pub close
context term
