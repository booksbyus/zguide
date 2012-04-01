//
// Reading from multiple sockets 
// This version uses zmq.Poll()
//
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
)

func main() {

	context, _ := zmq.NewContext()
	defer context.Close()

	//  Connect to task ventilator
	receiver, _ := context.NewSocket(zmq.PULL)
	defer receiver.Close()
	receiver.Connect("tcp://localhost:5557")

	//  Connect to weather server
	subscriber, _ := context.NewSocket(zmq.SUB)
	defer subscriber.Close()
	subscriber.Connect("tcp://localhost:5556")
	subscriber.SetSockOptString(zmq.SUBSCRIBE, "10001")

	pi := zmq.PollItems{
		zmq.PollItem{Socket: receiver, zmq.Events: zmq.POLLIN},
		zmq.PollItem{Socket: subscriber, zmq.Events: zmq.POLLIN},
	}

	//  Process messages from both sockets
	for {

		_, _ = zmq.Poll(pi, -1)

		switch {
		case pi[0].REvents&zmq.POLLIN != 0:
			//  Process task
			pi[0].Socket.Recv(0) // eat the incoming message
		case pi[1].REvents&zmq.POLLIN != 0:
			//  Process weather update
			pi[1].Socket.Recv(0) // eat the incoming message
		}

	}

	fmt.Println("done")

}
