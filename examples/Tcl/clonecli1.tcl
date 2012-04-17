#
# Clone client Model One
#

lappend auto_path .
package require KVSimple

zmq context context
set upd [zmq socket updates context SUB]
$upd setsockopt SUBSCRIBE ""
$upd connect "tcp://localhost:5556"
after 200

while {1} {
    set kvmsg [KVSimple new]
    $kvmsg recv $upd
    $kvmsg store kvmap
    puts [$kvmsg dump]
}

$upd close
context term
