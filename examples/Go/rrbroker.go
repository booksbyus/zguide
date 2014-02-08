// Simple request-reply broker
//
// Author:  Brendan Mc.
// Requires: http://github.com/alecthomas/gozmq

package main

import (
	zmq "github.com/alecthomas/gozmq"
)

func main() {
	context, _ := zmq.NewContext()
	defer context.Close()

	frontend, _ := context.NewSocket(zmq.ROUTER)
	backend, _ := context.NewSocket(zmq.DEALER)
	defer frontend.Close()
	defer backend.Close()
	frontend.Bind("tcp://*:5559")
	backend.Bind("tcp://*:5560")

	// Initialize poll set
	toPoll := zmq.PollItems{
		zmq.PollItem{Socket: frontend, Events: zmq.POLLIN},
		zmq.PollItem{Socket: backend, Events: zmq.POLLIN},
	}

	for {
		_, _ = zmq.Poll(toPoll, -1)

		switch {
		case toPoll[0].REvents&zmq.POLLIN != 0:
			parts, _ := frontend.RecvMultipart(0)
			backend.SendMultipart(parts, 0)

		case toPoll[1].REvents&zmq.POLLIN != 0:
			parts, _ := backend.RecvMultipart(0)
			frontend.SendMultipart(parts, 0)
		}
	}
}
