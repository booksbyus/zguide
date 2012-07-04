package require zmq

zmq context context
zmq socket client context REQ
client connect "tcp://localhost:5555"

for {set i 0} {$i < 10} {incr i} {
    zmq message msg -data "Hello @ [clock format [clock seconds]]"
    client send_msg msg
    msg close

    zmq message msg
    client recv_msg msg
    puts  "Received [msg data]/[msg size]"
    msg close
}

client close
context term

