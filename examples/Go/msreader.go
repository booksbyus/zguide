//
// Reading from multiple sockets 
// This version uses a simple recv loop
//
package main

import (
	"fmt"
	zmq "github.com/alecthomas/gozmq"
	"time"
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

	//  Process messages from both sockets
	//  We prioritize traffic from the task ventilator
	for {

		// ventilator
		for b, _ := receiver.Recv(zmq.NOBLOCK); b != nil; {
			// fake process task
		}

		// weather server
		for b, _ := subscriber.Recv(zmq.NOBLOCK); b != nil; {
			//  process task
			fmt.Printf("found weather =%s\n", string(b))
		}

		//  No activity, so sleep for 1 msec
		time.Sleep(1e6)
	}

	fmt.Println("done")

}
